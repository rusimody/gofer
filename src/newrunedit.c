static Void local runEditor() {		/* run editor on file lastEdit at  */
    static char editorCmd[100];		/* line editLine		   */
    String edt;
    Int    l,f;

    if ((edt = infProc ? INF_EDITLINE : fromEnv("EDITLINE",DEF_EDITLINE))
	    && lastEdit && lastLine && (l=substr("%d",edt))>=0
				    && (f=substr("%s",edt))>=0)
        if (l<f)
	    sprintf(editorCmd,edt,lastLine,lastEdit);
	else
	    sprintf(editorCmd,edt,lastEdit,lastLine);
    else
	if ((edt = infProc ? INF_EDITOR : fromEnv("EDITOR",DEF_EDITOR)))
	    if (lastEdit)
		sprintf(editorCmd,"(%s \"%s\")",edt,lastEdit);
	    else
		sprintf(editorCmd,"(%s)",edt);
	else {
	    ERROR(0) "No editor specified in environment variable EDITOR"
	    EEND;
	}

    if (infProc)
      printf("%s%s%s\n", infCh, editorCmd, infCh);
    else
      if (shellEsc(editorCmd)) {
	  ERROR(0) "Editor terminated abnormally"
	  EEND;
      }
}
