
Instructions for сompiling and attaching USERMATLIB.DLL 

Install Visual Studio first and then Intel fortran compiler. When installing the compiler, select "Integrate into Visual Studio". Supported versions can be found in the ANSYS documentation in the section on User Programmable Features (UPF). Add LIB and INCLUDE variables in the system environment. Create new solution and add new fortran dll project. The name of the created library must be "USERMATLIB.DLL"

COMPILATOR SETTINGS (*projectname*->properties). I use the following: 

Supress startup banner: 		Yes (/nologo)
Additional include Directories: 	C:\Program Files\ANSYS Inc\v***\ansys\customize\include
Optimization: 				Disable (/Od)
Preprocessor definitions: 		/DNOSTDCALL /DARGTRAIL /DPCWIN64_SYS /DPCWINX64_SYS /DPCWINNT_SYS /DCADOE_ANSYS /D__EFL /DFORTRAN /auto /c /Fo.\ /MD /W0
Debug information Format: 		Full (/debug:full)
Preprocess Source file: 		Yes (/fpp)
Preprocessor Definitions to fpp only: 	Yes (/noD)
Use Portlib Library: 			Yes (/4Yportlib)

LINKER SETTINGS
Enable incremental linking: 		No (/INCREMENTAL:NO)
Supress startup banner: 		Yes (/nologo)
Additional library Directories: 	C:\Program Files\ANSYS Inc\v***\ansys\custom\lib\winx64
Additional dependencies: 		ANSYS.LIB
Generate debug info: 			Yes (/DEBUG)

*** - your version of ANSYS
All other settings by default. Its allows me connect to ANSYS for debugging.

--------------------------///////--------------------------

After creation the dll file you have to connect this library to ANSYS:

1. Create environment variable named ANS_USER_PATH

My Computer->Properties->Advanced system settings->Advanced

    On the tab, click on the button:

Environment Variables->System Variables->New

2. In the variable value field, specify the path to the folder where library is located. Use only latin characters in the path.

    For example: C:\Username\......\Usermatlib
   
3. if everything is connected correctly in the ANSYS output window at startup there will be a line 

"User link path <ANS_USER_PATH>: *path to your folder*"

4 After launching the ANSYS, create an user material 

Preprocessor->Material Props->Material models

5. In the drop-down list of materials, select

Structural->Specialized Materials->User material options->user material

    And add cells. There should be 5 properties in total. Of which:

С1 - Young modulus
C2 - Puasson ratio
С3 - Yelding stress 
C4 - Strain hardening amplitude
C5 - Strain hardening exponent

That's all. Further we work as with the usual scheme.