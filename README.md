# Unity Platform

---

# Language version

---

Please note that the code is not compatible backwards with older Delphi versions. Due to language feature changes (inline variables among others), use only Delphi Rio and newer.

# Prepare to build

---

To prepare build, we must do the following:

1. Add `Win32` folder with `Bin` and `Dcu` subfolders.
1. Add `Win64` folder with `Bin` and `Dcu` subfolders (if x64 is the target).

Please note that x64 is only optional and can be skipped. Those folders are pre-configured in `Unity.dproj`. `Bin` folder holds compiled executable file along with Chromium files and `Dcu` folder holds compiled units.

The application requires additional folders and files in C:\ProgramData folder, thus:

1. Add folder `C:\ProgramData\Unity Platform`.
1. Copy Config.bin from Build folder to the `C:\ProgramData\Unity Platform`.
1. Add to the `C:\ProgramData\Unity Platform` subfolders (empty) `cache`, `coockies`, `layouts` and `sessions`. 
1. Copy Unity.inf to \Win32\bin\ (and/or \Win64\bin).

Or just run run `\build\make.ps1`.

# Configuration (Config.bin)

---

File Config.bin is default application configuration file deployed by the installer during installation on target machine. During application startup, `Config.cfg` is examined, and if missing it is copied from `Config.bin`. 
That way config file can be modified by the application without need for administration privilliges; this is because all the files deployed in ProgramData by the installer (that run as administrator) will always require
administrator privilliges, which would prevent application from modifying when run by the user.

# Custom configuration (*.cfg)

---

By default, application requires configuration file `Config.cfg`. However, it also accepts custom configuration file, to run with different file use the following command:

`Unity.exe config.cfg.ENV` 

Application accepts different config file as a first parameter. 

# Environment settings (Unity.inf)

---

Due to having two environments (PROD and TEST), we have two installation scripts, this also means that some settings cannot be kept in Config.bin, therefore we have Unity.inf that holds information about run environment, the file contains:

```
    [Environment]
    DataFolder=Unity Platform
    BinSource=Config.bin
    Setup=PROD
```

If the deployment targets PROD, those settings should remain unchanged. However, for TEST we should modify it:

```
    [Environment]
    DataFolder=Unity Platform Test
    BinSource=Config.bin.TEST
    Setup=TEST
```

`DataFolder` tells program to search settings in `C:\ProgramData\Unity Platform Test\`, while `BinSource` points to default configuration file; and finally `Setup` tells that this is TEST installation, 
and when the user run the application, then additional information "Test environment is enabled" will be displayed under the application caption.

For the convenience, in the `\bin` folder we may have two files: `Unity.inf` (PROD) and `Unity.inf.TEST` (TEST), then InnoSetup will compile installation program with source files and deploys it always as `Unity.inf`. 
Similarly, `Config.bin` file may be treated the same way; so that we can have two configuration files distinguished by the names and then InnoSetup will deploy it to the same file name. Installation scripts already takes it into account. 

# Environments

---

There are three environments setup:

1. Production.
1. For test users.
1. For developers.

Production environment uses the following:

1. https://unityapi.azurewebsites.net (API).
1. https://unityweb.azurewebsites.net (Web service).
1. citeam/1305_UnityDB_Prod (Database).

Testing version uses the following:

1. https://unityapi-unityapi-test.azurewebsites.net (Test API).
1. https://unityweb-unityweb-test.azurewebsites.net (Test web service).
1. citeam-dev/1305_UnityDB_Test (Database).

Development environment uses the following:

1. https://unityapi-unityapi-env.azurewebsites.net (Dev API).
1. https://unityweb-unityweb-env.azurewebsites.net (Dev web service).
1. citeam-dev/1305_UnityDB_Env (Database).

Both Production and Testing is deployed to the Software Centre for end-users and test-users. Development environment is only for developers.

# Microsoft Skype for Business (Lync.exe)

---

Unity Platform uses Skype API, it therefore requires following assemblies:

1. Microsoft.Lync.Controls.dll
1. Microsoft.Lync.Controls.Framework.dll
1. Microsoft.Lync.Model.dll
1. Microsoft.Lync.Utilities.dll
1. Microsoft.Office.Uc.dll

Additionally, there is LyncCall.exe file compiled separately (associated project in C#). This file must be placed in `bin` folder.

# Chromium Embedded

---

Unity Platform requires Chromium engine, the source is here:

[https://github.com/salvadordf/CEF4Delphi](https://github.com/salvadordf/CEF4Delphi)

The `bin` folder should contain:

1. chrome_elf.dll
1. d3dcompiler_47.dll
1. libcef.dll
1. libEGL.dll
1. libGLESv2.dll
1. natives_blob.bin
1. snapshot_blob.bin
1. v8_context_snapshot.bin
1. icudtl.dat
1. cef_sandbox.lib
1. libcef.lib
1. cef.pak
1. cef_100_percent.pak
1. cef_200_percent.pak
1. cef_extensions.pak
1. devtools_resources.pak

Add folder with theirs content:

1. locales
1. swiftshader
1. swiftshader

Additionally, there is SubProcess.exe which is compiled separately.

WARNING!

With RAD Studio 10.3 Rio - new CEF4Delphi, `SubProcess.exe` does not work properly, latest library `libcef.dll` (ver. 74.1.19.0) cannot be loaded, only older version works (ver. 3.x) works. 
The cause is unknown. There is no official fix and the issue has been closed: [https://github.com/salvadordf/CEF4Delphi/issues/167](https://github.com/salvadordf/CEF4Delphi/issues/167). 

The workaround is, build everything with latest CEF4Delphi and use latest binaries and libraries, except `SubProcess.exe` which must be compiled with  older RAD Studio Tokyo; 
then everything works as expected.

# Licence file (Unity.lic)

---

Licence file is encoded and it is a legacy file that stores basic information about distribution. This is because Unity Platform (formerly TR Tool 2017) was created as a private project (non-DFDS), 
as a proof of concept of a desktop client aiming to replace old Excel file automated with VBA. 

This file is scheduled for cancellation.
