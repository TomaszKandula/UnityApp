MAKEFILE
---------------------------------------------------------
.RES & .RC files:

1. Create RC file (plain text). 
2. Add files you want to place in resources:
   
   10 RCDATA "NunitoBold.ttf"

   Where:
   - 10 is ID handler.
   - RCDATA is data type (resources).
   - NunitoBold.ttf is file name that must be place in the same folder.  

   You can add as many files as you want. 
   
3. Use makefile.bat as follows:

   brcc32 -r -32 -fo RSDFILE.res RSDFILE.rc
   
   to compile RES file to be used in your project.
   
4. Usage in project:

   var RS: TResourceStream;
   
   RS:=TResourceStream.CreateFromID(hInstance, 10, RT_RCDATA);
   RS.SaveToFile(<dir>+'\'+<filename>);
   RS.free;   

5. In project file use directive R:

   {$R 'binres.res' 'binres.rc'}

   both RES and RC files must be in main directory.
   
   WARNING! RC file must always points to resources correctly:
   a. if in the same folder: "glyph.bmp"
   b. if in the other folder: "myfolder\\glyph.bmp" (use double backslash)

- END -

