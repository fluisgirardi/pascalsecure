unit security.manager.level.mgntdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ButtonPanel, ActnList, security.manager.mgntdlg, security.exceptions,
  security.manager.schema;

type
  TSecureLvlUsrMgntNotifyEvent = procedure(Sender:TObject; const aUser:TUserWithLevelAccess) of object;

  TSecureUsrLvlMgnt = class(TCustomUsrMgnt)
    ChangePass: TAction;
    addUser: TAction;
    delUser: TAction;
    DisableUser: TAction;
    ListView1: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure addUserExecute(Sender: TObject);
    procedure ChangePassExecute(Sender: TObject);
    procedure delUserExecute(Sender: TObject);
    procedure DisableUserExecute(Sender: TObject);
    procedure ListView1Editing(Sender: TObject; Item: TListItem;
      var AllowEdit: Boolean);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FOnAddUserClick: TNotifyEvent;
    FOnBlockUserClick: TSecureLvlUsrMgntNotifyEvent;
    FOnChangeUserClick: TSecureLvlUsrMgntNotifyEvent;
    FOnChangeUsrPassClick: TSecureLvlUsrMgntNotifyEvent;
    FOnDelUserClick: TSecureLvlUsrMgntNotifyEvent;
    procedure ValidateSelectedUser(const aUser: TListItem);
  published
    property OnAddUserClick:TNotifyEvent                       read FOnAddUserClick       write FOnAddUserClick;
    property OnDelUserClick:TSecureLvlUsrMgntNotifyEvent       read FOnDelUserClick       write FOnDelUserClick;
    property OnBlockUserClick:TSecureLvlUsrMgntNotifyEvent     read FOnBlockUserClick     write FOnBlockUserClick;
    property OnChangeUserClick:TSecureLvlUsrMgntNotifyEvent    read FOnChangeUserClick    write FOnChangeUserClick;
    property OnChangeUsrPassClick:TSecureLvlUsrMgntNotifyEvent read FOnChangeUsrPassClick write FOnChangeUsrPassClick;
  end;

  ESecurityInvalidUserSelected = class(ESecurityException)
  public
    constructor Create;
  end;

  resourcestring
    SInvalidUserSelected = 'Invalid user selected!';
var
  secureUsrLvlMgnt: TsecureUsrLvlMgnt;

implementation

{$R *.lfm}

constructor ESecurityInvalidUserSelected.Create;
begin
  inherited Create(SInvalidUserSelected);
end;

procedure TSecureUsrLvlMgnt.ValidateSelectedUser(const aUser:TListItem);
begin
  if (aUser=nil) or (aUser.Data=nil) or ((TObject(aUser.Data) is TUserWithLevelAccess)=false) then
    raise ESecurityInvalidUserSelected.Create;
end;

procedure TSecureUsrLvlMgnt.addUserExecute(Sender: TObject);
begin
  if Assigned(FOnAddUserClick) then
    FOnAddUserClick(Sender);
end;

procedure TSecureUsrLvlMgnt.ChangePassExecute(Sender: TObject);
begin
  ValidateSelectedUser(ListView1.Selected);

  if Assigned(FOnChangeUsrPassClick) then
    FOnChangeUsrPassClick(Sender, TUserWithLevelAccess(ListView1.Selected.Data));
end;

procedure TSecureUsrLvlMgnt.delUserExecute(Sender: TObject);
begin
  ValidateSelectedUser(ListView1.Selected);

  if Assigned(FOnDelUserClick) then
    FOnDelUserClick(Sender, TUserWithLevelAccess(ListView1.Selected.Data));
end;

procedure TSecureUsrLvlMgnt.DisableUserExecute(Sender: TObject);
begin
  ValidateSelectedUser(ListView1.Selected);

  if Assigned(FOnBlockUserClick) then
    FOnBlockUserClick(Sender, TUserWithLevelAccess(ListView1.Selected.Data));
end;

procedure TSecureUsrLvlMgnt.ListView1Editing(Sender: TObject; Item: TListItem;
  var AllowEdit: Boolean);
begin
  AllowEdit:=false;
end;

procedure TSecureUsrLvlMgnt.ListView1SelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  res: Boolean;
begin
  res:=(Item<>nil);
  delUser.Enabled:=res;
  ChangePass.Enabled:=res;
  DisableUser.Enabled:=res;
end;

end.

