import{t as e}from"../chunks/DK3Fl9T5.js";import{A as t,E as n,F as r,H as i,M as a,R as o,S as s,V as c,_ as l,b as u,g as d,h as f,j as p,k as m,m as h,p as g,v as _,x as v,z as y}from"../chunks/DCPVkMPI.js";import"../chunks/xihTtKlq.js";import{t as b}from"../chunks/Bin8K3vy.js";import{t as x}from"../chunks/qTVSSYEr.js";import{t as S}from"../chunks/De0q1msm.js";var C=e({load:()=>w,prerender:()=>!1});async function w({fetch:e,params:t}){let n=Number(t.id);if(!Number.isInteger(n)||n<=0)throw b(404,`Run not found: ${t.id}`);return{detail:await x(n,e)}}function T(e){return e.replaceAll(`&`,`&amp;`).replaceAll(`<`,`&lt;`).replaceAll(`>`,`&gt;`).replaceAll(`"`,`&quot;`).replaceAll(`'`,`&#39;`)}function E(e){if(!e)return``;try{let t=atob(e),n=Uint8Array.from(t,e=>e.charCodeAt(0));return new TextDecoder(`utf-8`,{fatal:!1}).decode(n)}catch{return e}}function D(e){return e==null?`N/A`:new Date(e*1e3).toLocaleString()}function O(e){return e.length===0?``:`<div class="headers">${e.map(e=>`<div class="header"><span class="dir">${T(e.direction)}</span><strong>${T(e.name)}:</strong> ${T(e.value)}</div>`).join(`
`)}</div>`}function k(e){return e.length===0?``:`<div class="bodies">${e.map(e=>{let t=e.truncated?` <em>(truncated)</em>`:``;return`<div class="body"><span class="dir">${T(e.direction)} body</span>${t}<pre>${T(E(e.content))}</pre></div>`}).join(`
`)}</div>`}function A(e){let t=e.error?`<p class="error">error: ${T(e.error)}</p>`:``;return`<details class="request">
  <summary>
    <span class="seq">#${e.seq}</span>
    <span class="method">${T(e.method)}</span>
    <span class="url">${T(e.url)}</span>
    <span class="status">${e.status_code??`—`}</span>
    <span class="dur">${e.duration_ms??`—`} ms</span>
  </summary>
  ${t}
  ${O(e.headers)}
  ${k(e.bodies)}
</details>`}var j=(e,t)=>`<dt>${T(e)}</dt><dd>${t}</dd>`;function M(e){let{run:t,requests:n}=e,r=n.map(A).join(`
`);return`<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>${T(t.name)} — Run detail</title>
<style>
:root { color-scheme: light dark; }
* { box-sizing: border-box; }
body { font-family: system-ui, -apple-system, "Segoe UI", Roboto, sans-serif; margin: 2rem auto; max-width: 60rem; padding: 0 1rem; color: #1f2937; }
header.page { display: flex; align-items: baseline; gap: 1rem; border-bottom: 1px solid #e5e7eb; padding-bottom: .75rem; }
h1 { margin: 0; font-size: 1.75rem; }
.badge { padding: .1rem .5rem; border-radius: 999px; font-size: .85em; }
.badge.running { background: #fff7e6; color: #b45309; }
.badge.completed { background: #ecfdf5; color: #047857; }
.badge.failed { background: #fef2f2; color: #b91c1c; }
.badge.cancelled { background: #f3f4f6; color: #4b5563; }
dl.meta { display: grid; grid-template-columns: max-content 1fr; gap: .25rem 1rem; margin: 1rem 0; }
dt { color: #6b7280; }
dd { margin: 0; }
details.request { border: 1px solid #e5e7eb; border-radius: .5rem; margin: .5rem 0; padding: .5rem .75rem; }
summary { cursor: pointer; display: flex; flex-wrap: wrap; gap: 1rem; font-family: monospace; }
.method { font-weight: 700; }
.url { color: #2563eb; overflow-wrap: anywhere; }
.seq { color: #6b7280; }
.status { font-weight: 600; }
.dir { display: inline-block; margin-right: .5rem; text-transform: uppercase; font-size: .8em; color: #6b7280; }
.header, .body { font-family: monospace; margin: .25rem 0; }
pre { white-space: pre-wrap; word-break: break-all; margin: .25rem 0 0; padding: .5rem; background: #f9fafb; border-radius: .25rem; }
.error { color: #b91c1c; font-family: monospace; }
@media (prefers-color-scheme: dark) { body { color: #e5e7eb; } pre { background: #1f2937; } details.request { border-color: #374151; } }
</style>
</head>
<body>
<header class="page">
  <h1>${T(t.name)}</h1>
  <span class="badge ${T(t.status)}">${T(t.status)}</span>
</header>
<dl class="meta">
  ${j(`Script path`,T(t.script_path))}
  ${j(`Started at`,D(t.started_at))}
  ${j(`Ended at`,D(t.ended_at))}
  ${j(`Concurrency`,String(t.concurrency))}
  ${j(`Rate limit`,String(t.rate_limit))}
  ${j(`Control socket`,T(t.control_socket))}
</dl>
<h2>Requests</h2>
${n.length?r:`<p>No requests recorded.</p>`}
</body>
</html>`}function N(e,t=`run.html`){let n=M(e),r=new Blob([n],{type:`text/html`}),i=URL.createObjectURL(r),a=document.createElement(`a`);a.href=i,a.download=t,document.body.appendChild(a),a.click(),a.remove(),URL.revokeObjectURL(i)}var P=_(`<p>No requests recorded.</p>`),F=_(`<p class="error svelte-1tucxyh"> </p>`),I=_(`<div class="kv svelte-1tucxyh"><span class="dir svelte-1tucxyh"> </span> <strong> </strong> </div>`),L=_(`<em>(truncated)</em>`),R=_(`<div class="body"><span class="dir svelte-1tucxyh"> </span> <!> <pre class="svelte-1tucxyh"> </pre></div>`),z=_(`<tr><td colspan="9"><!> <!> <!></td></tr>`),B=_(`<tr><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td> </td><td><button> </button></td></tr> <!>`,1),V=_(`<table><thead><tr><th>Seq</th><th>Method</th><th>URL</th><th>Status</th><th>Duration (ms)</th><th>Bytes in</th><th>Bytes out</th><th>Worker</th><th></th></tr></thead><tbody></tbody></table>`);function H(e,r){y(r,!0);let c=a({});function u(e){c[e]=!c[e]}var _=l(),b=t(_),x=e=>{d(e,P())},S=e=>{var a=V(),o=p(m(a));g(o,21,()=>r.requests,e=>e.id,(e,r)=>{var a=B(),o=t(a),l=m(o),_=m(l,!0);i(l);var y=p(l),b=m(y,!0);i(y);var x=p(y),S=m(x,!0);i(x);var C=p(x),w=m(C,!0);i(C);var T=p(C),D=m(T,!0);i(T);var O=p(T),k=m(O,!0);i(O);var A=p(O),j=m(A,!0);i(A);var M=p(A),N=m(M,!0);i(M);var P=p(M),V=m(P),H=m(V,!0);i(V),i(P),i(o);var U=p(o,2),W=e=>{var t=z(),a=m(t),o=m(a),c=e=>{var t=F(),a=m(t);i(t),n(()=>f(a,`error: ${s(r).error??``}`)),d(e,t)};h(o,e=>{s(r).error&&e(c)});var l=p(o,2);g(l,17,()=>s(r).headers,e=>e.id,(e,t)=>{var r=I(),a=m(r),o=m(a,!0);i(a);var c=p(a,2),l=m(c);i(c);var u=p(c);i(r),n(()=>{f(o,s(t).direction),f(l,`${s(t).name??``}:`),f(u,` ${s(t).value??``}`)}),d(e,r)}),g(p(l,2),17,()=>s(r).bodies,e=>e.id,(e,t)=>{var r=R(),a=m(r),o=m(a);i(a);var c=p(a,2),l=e=>{d(e,L())};h(c,e=>{s(t).truncated&&e(l)});var u=p(c,2),g=m(u,!0);i(u),i(r),n(e=>{f(o,`${s(t).direction??``} body`),f(g,e)},[()=>E(s(t).content)]),d(e,r)}),i(a),i(t),d(e,t)};h(U,e=>{c[s(r).id]&&e(W)}),n(()=>{f(_,s(r).seq),f(b,s(r).method),f(S,s(r).url),f(w,s(r).status_code??`—`),f(D,s(r).duration_ms??`—`),f(k,s(r).bytes_in??`—`),f(j,s(r).bytes_out??`—`),f(N,s(r).worker_id??`—`),f(H,c[s(r).id]?`hide`:`show`)}),v(`click`,V,()=>u(s(r).id)),d(e,a)}),i(o),i(a),d(e,a)};h(b,e=>{r.requests.length===0?e(x):e(S,-1)}),d(e,_),o()}u([`click`]);var U=_(`<header class="page-head svelte-1eidjaf"><h1 class="svelte-1eidjaf"> </h1> <button type="button">Download HTML</button></header> <dl><dt>Status</dt> <dd><!></dd> <dt>Script path</dt> <dd> </dd> <dt>Started at</dt> <dd> </dd> <dt>Ended at</dt> <dd> </dd> <dt>Concurrency</dt> <dd> </dd> <dt>Rate limit</dt> <dd> </dd> <dt>Control socket</dt> <dd> </dd></dl> <h2>Requests</h2> <!> <a href="/runs">&larr; Back to runs</a>`,1);function W(e,a){y(a,!0);let l=r(()=>a.data.detail.run);function u(){let e=(s(l).name||`run`).trim().replace(/[^\w.-]+/g,`_`)||`run`;N(a.data.detail,`${e}-${s(l).id}.html`)}var h=U(),g=t(h),_=m(g),b=m(_,!0);i(_);var x=p(_,2);i(g);var C=p(g,2),w=p(m(C),2);S(m(w),{get status(){return s(l).status}}),i(w);var T=p(w,4),E=m(T,!0);i(T);var D=p(T,4),O=m(D,!0);i(D);var k=p(D,4),A=m(k,!0);i(k);var j=p(k,4),M=m(j,!0);i(j);var P=p(j,4),F=m(P,!0);i(P);var I=p(P,4),L=m(I,!0);i(I),i(C),H(p(C,4),{get requests(){return a.data.detail.requests}}),c(2),n((e,t)=>{f(b,s(l).name),f(E,s(l).script_path),f(O,e),f(A,t),f(M,s(l).concurrency),f(F,s(l).rate_limit),f(L,s(l).control_socket)},[()=>new Date(s(l).started_at*1e3).toLocaleString(),()=>s(l).ended_at?new Date(s(l).ended_at*1e3).toLocaleString():`N/A`]),v(`click`,x,u),d(e,h),o()}u([`click`]);export{W as component,C as universal};