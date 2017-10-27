<b>PascalSECURE</b> is a securitysystem for the most popular standard controls of Lazarus

Brought to you by Fabio Luis Girardi and Andreas Frieß

<b>This is the stable repo.</b> If you are searching the development repo, take a look at https://github.com/afriess/pascalsecure/

Every securecontrol (Button, Edit, TabSheets,...) can activated/deactivated by free defined keys. You have to write only the logic depended on your need. Database, Textfiles, API,... all is possible.

These components allows you easy implement a security system on your application, GUI or console.
The basic usage of these components are:
  1) Insert a Security Manager. Choose one model (user level, user and authorizations, user/groups with authorizations...) that best fits on your needs, or write your own model using a customizable model.
  2) Insert a Security Interface (graphical, console or customized Login/UserManagement).
  3) If you are developing a GUI application, insert secure controls and set the property SecurityCode.

It is a standalone part of PascalSCADA 1.0 - A multiplatform SCADA framework for Lazarus. This code is a entire rewrite of PascalSCADA hosted at https://sourceforge.net/projects/pascalscada. The main goal of this initiative is:

Make security easy for normal applications
Extend the security system to all standard controls of Lazarus
Improve the translation, using .PO files instead of language defines
Improve the project modularization

This code is under modified LGPL (see LICENSE.modifiedLGPL.txt). This means that you can link this library inside your programs for any purpose. Only the included part of the code must remain LGPL.

If you make some improvements to this library, please create a new issue and attach the patch of your changes.

Contact : fabio at pascalscada dot com
