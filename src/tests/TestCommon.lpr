program TestCommon;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, TestSecurityManagerSchema, pascalsecure;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

