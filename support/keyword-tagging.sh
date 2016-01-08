#!/bin/bash

for Each ;
do case "${Each}" in
   *.erl | *.hrl )
     if git status "$Each"> /dev/null 2>&1 ;
     then File="${Each##*/}" ;
          Tag="${File//[^A-Za-z]/_}"

          Ident="-DIdent_${Tag}"
          Ident_format="${Ident}='\"\$Id: $File %h %ai %an \$\"'";
          git --no-pager log -1 --format="$Ident_format" "$Each" ;

          ( cd "${Each%/*}" && git remote -v ) \
          | while read Name Repos Rest ;
            do echo "-DGit_remote_${Tag}_${Name}='\"\$GitRemote: ${Repos} \$\"'"
            done | sort | uniq ;

          Commit_format="-DGit_commit_${Tag}='\"\$GitCommit: %H \$\"'" ;
          git log -1 --format="$Commit_format" "$Each" ;

          Branch=$(git status -s -b -u no)
          echo "-DGit_branch_${Tag}='\"\$GitBranch: ${Branch#* } \$\"'"
     else while read Line ;
          do Tag="${Line%%:*}" ;
             Value="${Line#*: }" ;
             case "$Tag" in
             URL | 'Last Changed Author' | 'Last Changed Rev' | 'Last Changed Date')
               declare "${Tag##* }"="${Value% (*}" ;;
             esac ;
          done < <(svn info "$Each") ;
          File="${Each##*/}" ;
          Ident="-DIdent_${File//[^A-Za-z]/_}" ;
          echo "${Ident}='\"\$Id: $Each $Rev $Date $Author \$\"'" ;
          Repos="-DURL_${File//[^A-Za-z]/_}" ;
          echo "${Repos}='\"\$URL: $URL \$\"'" ;
     fi ;;
   esac ;
done ;
