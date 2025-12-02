# -------------------------------------------------------------------------------------------------
# Copyright (c) 2024 Jules
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification, are permitted
# provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright notice, this list of conditions
#    and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice, this list of
#    conditions and the following disclaimer in the documentation and/or other materials provided
#    with the distribution.
#  * Neither the name of the zsh-syntax-highlighting contributors nor the names of its contributors
#    may be used to endorse or promote products derived from this software without specific prior
#    written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
# FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
# IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
# OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# -------------------------------------------------------------------------------------------------
#
# This is a zsh-syntax-highlighting highlighter for the hh200 language.
# It colors methods (GET, POST, etc) and keywords (then, HTTP, etc).
#

# Define default styles
: ${ZSH_HIGHLIGHT_STYLES[hh200:method]:=fg=cyan,bold}
: ${ZSH_HIGHLIGHT_STYLES[hh200:keyword]:=fg=magenta,bold}
: ${ZSH_HIGHLIGHT_STYLES[hh200:string]:=fg=yellow}
: ${ZSH_HIGHLIGHT_STYLES[hh200:url]:=fg=blue,underline}
: ${ZSH_HIGHLIGHT_STYLES[hh200:comment]:=fg=black,bold}

_zsh_highlight_highlighter_hh200_predicate() {
  # Enable the highlighter.
  # We could check if the buffer starts with specific commands if needed.
  return 0
}

_zsh_highlight_highlighter_hh200_paint() {
  local buf="$PREBUFFER$BUFFER"

  # We use zsh globbing/pattern matching to find tokens.
  # Using the 'z' flag to split into words for easier matching of keywords

  local word
  local words
  words=(${(z)buf}) # Split into shell words

  local current_pos=0
  for word in $words; do
      local len=${#word}

      # We need to find the actual position of this word in $buf to highlight it correctly
      # This is hard because ${(z)} strips whitespace.
      # So we search for the word starting from current_pos

      # Find word in buf starting at current_pos
      local rest="${buf:$current_pos}"
      local offset=${rest[(i)$word]} # Index of word in rest (1-based)

      if (( offset > ${#rest} )); then
        # Not found? Should not happen if split was correct
        break
      fi

      # calculate actual start index (0-based)
      local match_start=$(( current_pos + offset - 1 ))
      local match_end=$(( match_start + len ))

      # Check if word matches our patterns (Case insensitive for methods)
      # setopt localoptions nocasematch # This would affect all matches, lets use (#i) glob flag if possible

      if [[ "$word" == (#i)(GET|POST|PUT|DELETE|PATCH|OPTIONS|HEAD) ]]; then
          _zsh_highlight_add_highlight $match_start $match_end hh200:method
      elif [[ "$word" == (then|HTTP|Configs|Captures|Asserts) ]]; then
          _zsh_highlight_add_highlight $match_start $match_end hh200:keyword
      elif [[ "$word" == http* ]]; then
          _zsh_highlight_add_highlight $match_start $match_end hh200:url
      fi

      current_pos=$match_end
  done
}
