from json import dumps
from functools import reduce
from itertools import accumulate
from datetime import datetime
from random import randint
import functools, operator, csv, sys, time
import re

from loguru import logger
import httpx
from gevent import monkey, spawn, joinall

# Optional: Development
from hypothesis import given, strategies as st


# Prelude for parallelism. Internally patches Python built-in functions to use gevent.
monkey.patch_all()

# Patch logger format.
logger.remove()
# logger.add(sys.stderr, format="{level} {message}")
logger.add(sys.stderr, format="<level>{level: <8}</level> <cyan>{name}</cyan>:<cyan>{function}</cyan>:<cyan>{line}</cyan> <level>{message}</level>")

    # ??: class decorator
# Match Callable phase variables, i.e. {{lower_snake}}
class CallablePhase:
    r = re.compile(r'{{\s*([a-z_][a-z0-9_]*)\s*}}')

    @classmethod
    def compile(cls, context):
        def fn(template):
            def replace(match):
                key = match.group(1)
                return str(context.get(key, ''))
            return cls.r.sub(replace, template)
        return fn

# Match Flow phase variables, i.e. {{UPPER_SNAKE}}
class FlowPhase:
    r = re.compile(r'{{\s*([A-Z_][A-Z0-9_]*)\s*}}')

    @classmethod
    def compile(cls, context):
        def fn(template):
            def replace(match):
                key = match.group(1)
                return str(context.get(key, ''))
            return cls.r.sub(replace, template)
        return fn




def fmt(strs):
    if len(strs) == 0:
        return ""

    def h(rest, acc):
        if len(rest) == 0:
            return acc
        else:
            return h(rest[1:], f"{acc}:{rest[:1][0]}")
    return h(strs, "__🔬")  # marker hopefully unused by system under test

def json_dir_from_key(key):
    if key.startswith("__🔬:"):
        s = key[len("__🔬:"):]
    else:
        s = key
    res = list(s.split(':'))
    logger.info(f"lib res={res}")
    return res

def filter_marked_keys(defined):
    filtered = {k: v  for k, v in defined.items() if k.startswith("__🔬")}
    return list(filtered.keys())

def extract_nested(may_have_json, json_dir):
    def h(json_dir, child):
        maybe_first = json_dir[:1]  # either empty list or [first]
        if len(maybe_first) != 1:
            assert len(maybe_first) == 0
            return child
        else:
            first = maybe_first[0]
            if first == "0":
                logger.debug("unimplemented exception")
                first = 0
                
            child_first = child[first]

            return h(json_dir[1:], child_first)

    try:
        json = may_have_json.json()
        return h(json_dir, json)
    except:
        # ??: logger.info(may_have_json)
        return None


class Flow:
    name = ""
    callables = None
    responses = []
    defined = {}
    
    agent_username = ""

    def __init__(self, *args):
        self.callables = args

    def name(self, s):
        self.name = s
        return self

    def _contract_error(self, resp):
        agent_defined_dict = self.defined[self.agent_username]

        if len(resp.assertion_pt) > 0:
            sat = True
            info = ()
            # Our convention for `captured_name`s is UPPER_SNAKE.
            for captured_name, expect_value in resp.assertion_pt.items():
                if captured_name in agent_defined_dict:
                    got = agent_defined_dict[captured_name]
                    if str(resp.assertion_pt[captured_name]) != str(got):
                        info = (captured_name, got, expect_value)
                        sat = False
                        break
                else:
                    info = (captured_name, None, expect_value)
                    sat = False
                    break
            
            if sat:
                logger.success(f"asserted inline: {resp.assertion_pt.keys()}")
                is_err = False
            else:
                # summary[self.agent_username]['error_count'] += 1  # ??: unify with check_responses
                # summary[self.agent_username]['error_trace'].append("contract_error")
                logger.error(f"{info[0]}: {info[1]}, expected {info[2]}")
                is_err = True
            return is_err

    def repeat_until(self, resp, client, httpx_req, resp_assertion_pt, MAX=50):
        cp_will_check = resp.will_check
        cp_assertion_pt = resp.assertion_pt
        
        def keep_run_attrs(resp):
            resp.will_check = cp_will_check
            resp.assertion_pt = cp_assertion_pt
            return resp

        def do(resp, n):
            if n >= MAX or (not self._contract_error(resp)):
                return resp
            else:
                new_resp = keep_run_attrs(client.send(httpx_req))
                return do(new_resp, n + 1)
        return do(resp, 0)

    # Sequentially run callables in Flow, updating defined variables.
    def run(self, agent_username):
        self.agent_username = agent_username
        self.defined[agent_username] = {}

        def eval(s):
            fmt = s
            for key, value in self.defined[agent_username].items():
                placeholder = "{{" + key + "}}"
                fmt = fmt.replace(placeholder, str(value))
            return fmt

        def eval_body(tup):
            vars = self.defined[agent_username]
            fmt = FlowPhase.compile(vars)
            def rec_fmt(obj_body):
                def h(arg):
                    match arg:
                        case list():
                            return [h(item) for item in arg]
                        case dict():
                            return {h(k): h(v) for k, v in arg.items()}
                        case str():
                            return fmt(arg)
                        case _:
                            return arg

                return h(obj_body)

            match tup:
                case ("json_d", obj_body):
                    evaled = rec_fmt(obj_body)
                    logger.info(f"evaled={evaled}")
                    return [evaled]
                case ("json", obj_body):
                    return rec_fmt(obj_body)
                case ("form-data", d):
                    return rec_fmt(d)
                case _:
                    return tup

        with httpx.Client() as client:
            def send_requests_or_panic(c):
                # ??: upload file
                # httpx_req = client.build_request(eval(c.method), eval(c.url), data=c.body, files={'file': open("half-megabytes-image.png", "rb")})

                match c.body:
                    case (None, c_body):
                        httpx_req = client.build_request(eval(c.method), eval(c.url),
                                                         data=c_body)
                    case _:
                        httpx_req = client.build_request(eval(c.method), eval(c.url),
                                                         json=eval_body(c.body))
                # logger.debug(f"content\n{httpx_req.content}")  # comment but don't delete

                for k, v in c.flow_headers.items():
                    httpx_req.headers[k] = eval(v)  # For example, {{TOKEN}} gets evaluated here during Flow.run(...)
                resp = client.send(httpx_req)
                
                # Sets up response list for general checks in `check_responses`.
                # Mark and/or parametrize special checks here.
                resp.will_check = c.resp_will_check
                resp.assertion_pt = c.resp_assertion_pt
                resp.expect_code = c.resp_expect_code

                if c.resp_is_repeat_block:
                    resp = self.repeat_until(resp, client, httpx_req, c.resp_assertion_pt, MAX=3)

                self.responses.append(resp)
                return resp

            for c in self.callables:
                resp = send_requests_or_panic(c)
                for var_name, var_value in (c.flow_captures | c.flow_cmp).items():
                    if isinstance(var_value, list):
                        self.defined[agent_username][var_name] = extract_nested(resp, var_value)
                    else:
                        self.defined[agent_username][var_name] = var_value

        return self

    def check_responses(self):
        agent_defined_dict = self.defined[self.agent_username]

        def before_after_relation_error(resp, violating_relation_fn, summary):
            check_list = list(map(json_dir_from_key, filter_marked_keys(self.defined[self.agent_username])))
            for check_json in check_list:
                if resp.will_check:
                    got = extract_nested(resp, check_json)
                    before = self.defined[self.agent_username][fmt(check_json)]

                    # Default violating_relation_fn, for example, returns whether
                    # !(got == before), a violation of before-after relation.
                    if violating_relation_fn(got, before):
                        summary[self.agent_username]['error_count'] += 1
                        summary[self.agent_username]['error_trace'].append("before_after_relation_error")
                        logger.error(f"Flow: {self.name}\n{resp.request}\n\t{resp.content}")
                    else:
                        logger.success(f"Flow: {self.name}\tbefore={before},got={got}")


        def status_code_error(resp, summary, expect_code=200):
            if hasattr(resp, "expect_code"):
                expect_code = resp.expect_code

            logger.info(resp.url)

            if resp.status_code != expect_code:
                summary[self.agent_username]['error_count'] += 1
                summary[self.agent_username]['error_trace'].append("status_code_error")
                logger.error(f"Flow: {self.name} {resp.status_code}\n{resp.request}\n\t{resp.content}")
            else:
                logger.success(f"expect_code={expect_code},got={resp.status_code}")

        def contract_error(resp, summary):
            if len(resp.assertion_pt) > 0:
                sat = True
                info = ()
                # Our convention for `captured_name`s is UPPER_SNAKE.
                for captured_name, expect_value in resp.assertion_pt.items():
                    if captured_name in agent_defined_dict:
                        got = agent_defined_dict[captured_name]
                        if str(resp.assertion_pt[captured_name]) != str(got):
                            info = (captured_name, got, expect_value)
                            sat = False
                            break
                    else:
                        info = (captured_name, None, expect_value)
                        sat = False
                        break
                
                if sat:
                    logger.success(f"asserted inline: {resp.assertion_pt.keys()}")
                else:
                    summary[self.agent_username]['error_count'] += 1
                    summary[self.agent_username]['error_trace'].append("contract_error")
                    logger.error(f"{info[0]}: {info[1]}, expected {info[2]}")
                    

        summary = { self.agent_username: {
            # "total_hits": len(self.responses),  # ??: increment in httpx client block
            "error_count": 0,
            "error_trace": []
            }}

        def update_summary(i, resp):
            status_code_error(resp, summary)
            before_after_relation_error(resp, operator.eq, summary)
            contract_error(resp, summary)

        for i, resp in enumerate(self.responses):
            update_summary(i, resp)

        return summary

class Callable:
    url = ""
    body = {}
    apply_draft = {}
    
    flow_captures = {}
    flow_headers = {}
    flow_cmp = {}
    
    resp_will_check = False
    resp_assertion_pt = {}
    resp_is_repeat_block = False
    resp_expect_code = 200

    def __init__(self, method, url, body, headers={}):
        self.method = method
        self.url = url
        self.body = body
        self.flow_headers = headers

        # Apply non-mandatory fields draft values; commitment is required from
        # application code by calling `end()`.
        self.apply({
            "yyyymmdd":    datetime.now().date().strftime("%Y-%m-%d"),
            "yyyymm01": f"{datetime.now().date().strftime("%Y-%m-%d")[:-2]}01",
        })

    def end(self):
        self.url = self.apply_draft["url"]
        self.body = self.apply_draft["body"]
        return self

    def apply(self, vars):
        with_vars = CallablePhase.compile(vars)

        # Replace {{variables}} in URL
        processed_url = self.url
        for key, value in vars.items():
            placeholder = "{{" + key + "}}"
            processed_url = processed_url.replace(placeholder, str(value))
        self.apply_draft["url"] = processed_url
        
        # Replace {{variables}} in body
        def dict_with_vars(body):
            processed_body = {}
            for key, value in body.items():
                if isinstance(value, str):
                    processed_body[key] = with_vars(value)
                else:
                    processed_body[key] = value
            return processed_body

        match self.body:
            case ("form-data", {"data": value}):
                self.apply_draft["body"] = ("form-data", {"data": value})
            case (ty, body):
                self.apply_draft["body"] = (ty, dict_with_vars(body))

        return self
    
    def capture(self, json_dir):
        self.flow_captures = json_dir
        return self

    def cmp(self, cmp_list):
        self.flow_cmp = cmp_list
        return self
    
    def mark(self):
        self.resp_will_check = True
        return self

    # ??: will merge with asserts/2
    def asserts_defined(self, contracts):
        if isinstance(contracts, tuple) and contracts[0] == "repeat_until":
            self.resp_is_repeat_block = True
            self.resp_assertion_pt = contracts[1]
            # contracts[2] # ?? bump_phone_number
        else:
            self.resp_assertion_pt = contracts

        return self
    
    def asserts(self, contracts):
        match contracts:
            case {"status_code": n}:
                self.resp_expect_code = n
            case {"were_captured": d}:
                self.resp_assertion_pt = d
        return self
    
    def assert_captured(self, d):
        return self.asserts({"were_captured": d})



def comparing(cmp_values):
    cmp_dict = {}
    for array in cmp_values:
        cmp_dict[fmt(array)] = array
    return cmp_dict

def mk_agent(fns):
    def agent(user_row):
        # Each agent runs modules sequentially.
        for fn in fns:
            print_if_error(fn(user_row))
    return agent

# Expect N_AGENTS * N_FLOWS summary print statements.
def print_if_error(agent_summary):
    username = list(agent_summary.keys())[0]
    count = agent_summary[username]['error_count']

    if count == 0:
        emoji_str = "🎉 "
        agent_summary = {}
    else:
        emoji_str = ""
    logger.info(f"{emoji_str}Unexpected results: {agent_summary}")


################
#  Decorators  #
################

def module_flow(f):
    @functools.wraps(f)
    def w(*args, **kwargs):
        user_row = args[0]
        logger.info(f"agent user_row={user_row}")
        flow = f(*args, **kwargs)
        return flow.name(f.__name__).run(user_row['username']).check_responses()
    return w

# Immediately invoke decorated function definition.
def main(f):
    @functools.wraps(f)
    def w(*args, **kwargs):
        f(*args, **kwargs)
    w()


###############################################################
#  Hypothesis: each building block doesn't crash in its own.  #
###############################################################

@given(st.lists(st.text()))
def test_fmt(arg):
    assert isinstance(fmt(arg), str)

@given(st.text())
def test_json_dir_from_key(arg):
    assert isinstance(json_dir_from_key(arg), list)

@given(st.lists(st.text()))
def test_json_dir_roundtrip(arg):
    assert isinstance(json_dir_from_key(fmt(arg)), list)

    
if __name__ == "__main__":
    test_fmt()
    test_json_dir_from_key()
    test_json_dir_roundtrip()
