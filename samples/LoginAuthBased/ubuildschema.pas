unit ubuildschema;

{$mode objfpc}{$H+}

{$Define LocalFile}

interface

uses
  Classes, SysUtils, security.manager.schema;

function BuildSchemaLVL(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;
function BuildSchemaUser(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;
function BuildSchemaUserFromDB(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;


implementation

uses
  LazLogger,
  LazUtils,
  LazFileUtils,
  BufDataSet,
  db
  ;

function BuildSchemaLVL(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;
begin
  result := false;
  ASchemaTyp:= TUsrLevelMgntSchema.UsrMgntType;
  ASchema:=TUsrLevelMgntSchema.Create(1, 100, 1);
  with ASchema as TUsrLevelMgntSchema do begin
    UserList.Add(0,TUserWithLevelAccess.Create(0,'root','1','Main administrator',false, 1));
    UserList.Add(1,TUserWithLevelAccess.Create(1,'andi','2','A user',            false, 1));
    UserList.Add(2,TUserWithLevelAccess.Create(2,'user','3','Another user',      false, 10));
  end;
end;

function BuildSchemaUser(var ASchemaTyp: TUsrMgntType;var ASchema: TUsrMgntSchema):Boolean;
var
  AUser: TAuthorizedUser;
begin
  result := false;
  ASchemaTyp:= TUsrAuthSchema.UsrMgntType;
  ASchema:=TUsrAuthSchema.Create;
  with (ASchema as TUsrAuthSchema) do begin
    //                +this should be equal to-+
    //                ⬇                        ⬇
    Autorizations.Add(0, TAuthorization.Create(0,'autorizacao1'));
    Autorizations.Add(1, TAuthorization.Create(1,'autorizacao2'));
    Autorizations.Add(2, TAuthorization.Create(2,'autorizacao3'));
    Autorizations.Add(3, TAuthorization.Create(3,'autorizacao4'));
  end;
  // root
  //this one should be equal     ⬇
  AUser:= TAuthorizedUser.Create(0,'root','1','administrator',false);
  AUser.AuthorizationList.Add(0,(ASchema as TUsrAuthSchema).Autorizations.KeyData[0]);
  AUser.AuthorizationList.Add(1,(ASchema as TUsrAuthSchema).Autorizations.KeyData[1]);
  AUser.AuthorizationList.Add(2,(ASchema as TUsrAuthSchema).Autorizations.KeyData[2]);
  AUser.AuthorizationList.Add(3,(ASchema as TUsrAuthSchema).Autorizations.KeyData[3]);
  //with this one                      ⬇
  TUsrAuthSchema(ASchema).UserList.Add(0,AUser);


  // andi
  AUser:= TAuthorizedUser.Create(1,'andi','2','User Andi',false);
  AUser.AuthorizationList.Add(0,(ASchema as TUsrAuthSchema).Autorizations.KeyData[0]);
  AUser.AuthorizationList.Add(1,(ASchema as TUsrAuthSchema).Autorizations.KeyData[1]);
  AUser.AuthorizationList.Add(2,(ASchema as TUsrAuthSchema).Autorizations.KeyData[2]);
  TUsrAuthSchema(ASchema).UserList.Add(1,AUser);

  // user
  AUser:= TAuthorizedUser.Create(2,'user','3','Another User',false);
  AUser.AuthorizationList.Add(0,(ASchema as TUsrAuthSchema).Autorizations.KeyData[0]);
  TUsrAuthSchema(ASchema).UserList.Add(2,AUser);
end;

function BuildSchemaUserFromDB(var ASchemaTyp: TUsrMgntType;
  var ASchema: TUsrMgntSchema): Boolean;
var
  BDS: TBufDataset;
  FileName: String;
  Key: String;
  KeyIndex: Integer;
  aUser: TCustomUser;
begin
  DebugLnEnter({$I %FILE%},{$I %LINE%},'BuildSchemaUserFromDB');
  Result:= False;
  // Create a basic schema
  { TODO -oANdi : What should we if an old Schema exists ?}
  // if no Schema exist, create a new one
  if not Assigned(ASchema) then begin
    ASchemaTyp:= TUsrAuthSchema.UsrMgntType;
    ASchema:=TUsrAuthSchema.Create;
  end;
  {$IFNDEF LocalFile}
  FileName:= ChangeFileExt(LazFileUtils.GetAppConfigFileUTF8(False, False, True),'.bds'); ;
  DebugLn('Filename=',FileName);
  {$ELSE LocalFile}
  FileName:= ChangeFileExt(paramstr(0),'.bds'); ;
  DebugLn('Local Filename=',FileName);
  {$ENDIF LocalFile}
  BDS:= TBufDataset.Create(nil);
  try
    if not FileExists(FileName) then begin
      DebugLn('File not exists -> Create an empty one');
      BDS.FieldDefs.Add('UserID',ftInteger);
      BDS.FieldDefs.Add('UserName',ftString,50);
      BDS.FieldDefs.Add('UserPassword',ftString,50);
      BDS.FieldDefs.Add('UserActive',ftBoolean);
      BDS.FieldDefs.Add('AuthID',ftInteger);
      BDS.FieldDefs.Add('AuthName',ftString,50);
      BDS.FieldDefs.Add('AuthActive',ftBoolean);
      BDS.CreateDataset;
      BDS.Open;
      BDS.Append;
      BDS.FieldByName('UserID').AsInteger:= 0;
      BDS.FieldByName('UserName').AsString:= 'root';
      BDS.FieldByName('UserPassword').AsString:= '1';
      BDS.FieldByName('UserActive').AsBoolean:= True;
      BDS.FieldByName('AuthID').AsInteger:= 0;
      BDS.FieldByName('AuthName').AsString:= 'admin';
      BDS.FieldByName('AuthActive').AsBoolean:= True;
      BDS.Post;
      BDS.SaveToFile(FileName);
      BDS.Close;
    end;
    if not FileExists(FileName) then begin
      DebugLn('something goes wrong - file not found');
      exit;
    end;
    DebugLn('Load file');
    BDS.LoadFromFile(FileName);
    BDS.First;
    while not BDS.EOF do begin
      // First step: search for the user
      Key := BDS.FieldByName('UserName').AsString;
      aUser := TUsrAuthSchema(MySchema).UserByName[Key];
      if (aUser = nil) then begin
        // User not found
        aAUser:= TAuthorizedUser.Create(BDS.FieldByName('UserID').AsInteger,
                                        Key,
                                        BDS.FieldByName('UserPassword').AsString,
                                        Key,
                                        BDS.FieldByName('UserActive').AsBoolean);

      end;
      //AAuthorization:= TAuthorization.Create(0,'autorizacao1');
      //AUser.AuthorizationList.Add(0,AAuthorization);
      //AAuthorization:= TAuthorization.Create(0,'autorizacao2');
      //AUser.AuthorizationList.Add(0,AAuthorization);
      //TUsrAuthSchema(ASchema).UserList.Add(0,AUser);



      BDS.Next;
    end;
  finally
    FreeAndNil(BDS);
    DebugLnExit('function finished');
  end;
  Result:= true;
end;




end.

