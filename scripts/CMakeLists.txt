list(APPEND scripts_files
  exprep_post.sh
  )

install(PROGRAMS ${scripts_files}
        DESTINATION "scripts")
