unit pascalsecure_visual_reg;
{
    This file is part of the PascalSECURE Project 2017
    Copyright (c) Fabio Luis Girardi

    See the files COPYING.LGPL and COPYING.modifiedLGPL,
    included in this distribution.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

    It is a standalone part of PascalSCADA 1.0
       - A multiplatform SCADA framework for Lazarus.
--------------------------------------------------------------------------------
 Contibutors:

 ******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  FormEditingIntf,
  ProjectIntf,
  LazIDEIntf,
  security.controls.bitbtn,
  security.controls.SecureButton,
  security.controls.SecureTabsheet;

ResourceString
  SPascalSecurePalette = 'PascalSecure_Controls';

procedure Register;

implementation

{$R graphical.res}

procedure Register;
begin
  RegisterComponents(SPascalSecurePalette, [TSecureButton,
                                            TSecureBitBtn,
                                            TSecureTabSheet]);

end;

end.



