DFM Check
=========
If you work with frames and form inheritance you may sometimes experience that the
application doesn't start or one of the derived forms crashes with an EReadError.
This is because a control in the frame or base form was deleted or renamed. Unfortunatelly
the DFM files are resource files that do not go through the compiler for validation.

This IDE plugin adds the possibility to check all DFM files of the active project. It
generates a special unit that accesses every control on the formular by code. This unit
will fail to compile if one or more DFM files do not fit to the form class. When the plugin
is installed there is a new menu item "DFM Check" in the "Project" menu that invokes
the DFM validation. You can also use the "Open/Close all forms" menu item to let the IDE
validate the forms. But this requires a lot more resources and time but is useful to
automatically save modifications due to parent component changes in the base class DFM.

The code of the special unit is never linked into the executable because of Delphi's
smart linking feature.


DFMCheck.exe (Delphi Projects only)
===================================
The DFMCheck.exe is a command line tool that generates a <Project>_DfmCheck_Unit.pas
and copies the project file to <Project>_DfmCheck.[dpr/cfg/dproj]. This allows a build
script to invoke the compiler on the DfmCheck project to verify the correctness of the
DFM files.


Installation
============
The DfmCheck.bpl is a hybrid package. It can be used as a DLL Expert or as a
design-time package. Using it as a design-time package could cause problems with the
package unloading during compilation. But it is easier to install because you must
only add the BPL to the package list. (Menu "Components/Install Packages")

Installing the BPL as an DLL Expert can be done by using the Expert Manager from
GExperts (http://www.gexperts.org) or by adding the BPL by hand to the registry key
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
HKCU\{Borland|CodeGear|Embarcadero}\{Delphi|BDS|RAD Studio}\{version}\Experts
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Supported Delphi Versions
=========================
- Delphi/BCB 5
- Delphi/BCB 6
- Delphi 7
- Delphi 2005 (9)
- BDS 2006 (10)
- Delphi 2007, C++Builder 2007, RAD Studio 2007
- Delphi/C++Builder/RAD Studio 2009-10.3
