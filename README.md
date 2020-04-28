# Unity Platform

---

# Language version

---

The code is not compatible backwards with older Delphi versions. Due to language feature changes (inline variables among others), use only Delphi Rio and newer.

# Build

---

To prepare build, we must do the following:

1. Add `Win32` folder with `Bin` and `Dcu` subfolders.
1. Add `Win64` folder with `Bin` and `Dcu` subfolders (if x64 is the target).
1. Add folder `C:\ProgramData\Unity Platform`.
1. Copy Config.bin from Build folder to the `C:\ProgramData\Unity Platform`.
1. Add to the `C:\ProgramData\Unity Platform` subfolders (empty) `cache`, `coockies`, `layouts` and `sessions`. 

To execute it, please run `\build\make.ps1`.

File Config.bin is default application configuration file deployed by the installer during installation on target machine. During application startup, `Config.cfg` is examined, and if missing it is copied from `Config.bin`. 
That way config file can be modified by the application without need for administration privilliges; this is because all the files deployed in ProgramData by the installer (that run as administrator) will always require
administrator privilliges, which would prevent application from modifying when run by the user.

# Custom config file

---

By default, application requires configuration file `Config.cfg`. However, it also accepts custom configuration file, to run with different file use the following command:

`Unity.exe config.cfg.ENV` 

Application accepts different config file as a first parameter. 


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
