#!/usr/bin/env bash
#  SPDX-License-Identifier: GPL-3.0-or-later
#----------------------------------------------------------------------
#  pla-util - A power line adapter utility
#  Copyright (C) 2016-2022 John Serock
#
#  This file is part of pla-util.
#
#  pla-util is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  pla-util is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program. If not, see <http://www.gnu.org/licenses/>.
#----------------------------------------------------------------------
__pla_util_add_trailing_space () {
  local i
  for (( i=0; i < ${#COMPREPLY[@]}; i++ )); do
    case ${COMPREPLY[${i}]} in
      ( check-* | set-* | get-hfid | get-network-info )
        COMPREPLY[${i}]+=" "
        ;;
    esac;
  done
}

__pla_util_current_command_position () {
  local i j word
  for (( i=${#COMP_WORDS[@]}-1; i > 0; i-- )); do
    word=${COMP_WORDS[${i}]}
    for (( j=0; j < ${#all_cmds[@]}; j++ )); do
      if [ "${word}" == "${all_cmds[${j}]}" ]; then
        cur_cmd_pos=${i}
        break 2
      fi
    done
  done
}

_pla_util_completions () {
  local -a -r all_cmds=("check-dak" "check-nmk" "discover" "get-capabilities" "get-discover-list" "get-hfid" "get-id-info" "get-network-info" "get-network-stats" "reset" "restart" "set-hfid" "set-nmk")
  local -a -r primary_opts=("--interface=" "--pla=" "--timeout=")
  local -a words=()
  local cur_cmd_pos=0
  __pla_util_current_command_position
  if [ ${#COMP_WORDS[@]} -eq 2 -a ${cur_cmd_pos} -eq 0 ]; then
    words=("--help" "--version" ${primary_opts[@]} ${all_cmds[@]})
  elif [ "$3" == "get-hfid" ]; then
    words=("manufacturer" "user")
  elif [ "$3" == "get-network-info" ]; then
    words=("any" "member")
  elif [ "$3" == "--help" -o "$3" == "-h" ]; then
    :
  elif [ "$3" == "--version" -o "$3" == "-V" ]; then
    :
  elif [ ${COMP_POINT} -gt 0 -a "${COMP_LINE:${COMP_POINT}-1:1}" == "=" ]; then
    :
  elif [ ${cur_cmd_pos} -eq 0 -a ${COMP_CWORD} -gt 0 ]; then
    words=(${primary_opts[@]} ${all_cmds[@]})
  elif [ ${cur_cmd_pos} -ne 0 -a ${COMP_CWORD} -le ${cur_cmd_pos} ]; then
    words=(${primary_opts[@]})
  else
    :
  fi
  COMPREPLY=($(compgen -o nosort -o nospace -W "${words[*]}" -- "$2"))
  __pla_util_add_trailing_space
  return 0
}

complete -o nosort -o nospace -F _pla_util_completions pla-util
