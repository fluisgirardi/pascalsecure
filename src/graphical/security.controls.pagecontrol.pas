{
    This file is part of the Extended Tabsheet
	It is a fork of the TPageControl of the Lazarus-project
	to include the secure_tabsheet.
	
	
    Some parts Copyright (c) 2016 Andreas Frieß.

    See the files COPYING.LGPL and COPYING.modifiedLGPL, 
    included in this distribution.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

--------------------------------------------------------------------------------
 Contibutors: 
    
 ******************************************************************************}
unit security.controls.pagecontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Controls, security.controls.SecureTabsheet;

type
  { TSecurePageControl }

  TSecurePageControl = class(TCustomTabControl)
  private
    FPageToUndock: TSecureTabSheet;
    function GetActivePageIndex: Integer;
    function GetActiveTabSheet: TSecureTabSheet;
    function GeTSecureTabSheet(Index: Integer): TSecureTabSheet;
    procedure SetActivePageIndex(const AValue: Integer);
    procedure SetActiveTabSheet(const AValue: TSecureTabSheet);
    function FindPageWithDockClient(Client: TControl): TSecureTabSheet;
  protected
    class procedure WSRegisterClass; override;
    function GetPageClass: TCustomPageClass; override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
                       State: TDragState; var Accept: Boolean); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function DoUndockClientMsg(NewTarget, Client: TControl):boolean; override;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
  public
    function FindNextPage(CurPage: TSecureTabSheet;
                          GoForward, CheckTabVisible: Boolean): TSecureTabSheet;
    procedure SelectNextPage(GoForward: Boolean);
    procedure SelectNextPage(GoForward: Boolean; CheckTabVisible: Boolean);
    function AddTabSheet: TSecureTabSheet;
    property ActivePageIndex: Integer read GetActivePageIndex
                                      write SetActivePageIndex;
    property Pages[Index: Integer]: TSecureTabSheet read GeTSecureTabSheet;
  published
    property ActivePage: TSecureTabSheet read GetActiveTabSheet write SetActiveTabSheet;
    property OnGetDockCaption;

    property Align;
    property Anchors;
    property BorderSpacing;
    property BiDiMode;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    //property HotTrack;
    property Images;
    property MultiLine;
    //property OwnerDraw;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    //property RaggedRight;
    //property ScrollOpposite;
    property ShowHint;
    property ShowTabs;
    //property Style;
    //property TabHeight;
    property TabIndex;
    property TabOrder;
    property TabPosition;
    property TabStop;
    //property TabWidth;
    property Visible;
    property OnChange;
    property OnChanging;
    property OnCloseTabClicked;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    //property OnDrawTab;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property Options;
  end;

procedure Register;

implementation

uses
  WSComCtrls, LResources, InterfaceBase;

procedure Register;
begin
  RegisterComponents('Secure TabSheet',[TSecurePageControl]);
end;

{******************************************************************************
                                  TSecurePageControl
 ******************************************************************************

  Author: Mattias Gaertner

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

}

{ TSecurePageControl }

function TSecurePageControl.GetActivePageIndex: Integer;
begin
  Result:=inherited PageIndex;
end;

function TSecurePageControl.GetActiveTabSheet: TSecureTabSheet;
begin
  Result:=TSecureTabSheet(inherited ActivePageComponent);
end;

function TSecurePageControl.GeTSecureTabSheet(Index: Integer): TSecureTabSheet;
begin
  Result:=TSecureTabSheet(inherited Page[Index]);
end;

procedure TSecurePageControl.SetActivePageIndex(const AValue: Integer);
begin
  inherited PageIndex:=AValue;
end;

procedure TSecurePageControl.SetActiveTabSheet(const AValue: TSecureTabSheet);
begin
  //debugln(['TSecurePageControl.SetActiveTabSheet ',DbgSName(Self),' ',DbgSName(AValue)]);
  ActivePageComponent := AValue;
end;

function TSecurePageControl.FindPageWithDockClient(Client: TControl): TSecureTabSheet;
var
  i: integer;
begin
  for i := 0 to PageCount - 1 do
    if Pages[i] = Client.Parent then
    begin
      Result := Pages[i];
      exit;
    end;
  Result := nil;
end;

class procedure TSecurePageControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterPageControl;
  RegisterPropertyToSkip(TSecurePageControl, 'PageIndex', 'Needed when converting from VCL TabbedNotebook to TSecurePageControl', '');
  RegisterPropertyToSkip(TSecurePageControl, 'TabFont', 'Needed when converting from VCL TabbedNotebook to TSecurePageControl', '');
end;

function TSecurePageControl.GetPageClass: TCustomPageClass;
begin
  Result := TSecureTabSheet;
end;

procedure TSecurePageControl.DoAddDockClient(Client: TControl; const ARect: TRect);
var
  TabSheet: TSecureTabSheet;
begin
  // Client cannot be added to TSecurePageControl itself but new TabSheet should be
  // added and client placed onto it
  TabSheet := TSecureTabSheet.Create(Self);
  TabSheet.Caption := GetDockCaption(Client);
  try
    TabSheet.PageControl := TPageControl(Self);
    Client.Parent := TabSheet;
    // delphi compatible behaviour => align to client
    Client.Align := alClient;
  except
    FreeAndNil(TabSheet);
  end;
end;

procedure TSecurePageControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  P: TPoint;
begin
  P := Parent.ClientToScreen(Point(Left, Top));
  Source.DockRect := Rect(P.X, P.Y, P.X + Width, P.Y + Height);
  DoDockOver(Source, X, Y, State, Accept);
end;

procedure TSecurePageControl.DoRemoveDockClient(Client: TControl);
begin
  // we cannot search for client page here since Client.Parent
  // is changed at moment => search for page before parent change and free here
  FreeAndNil(FPageToUndock);
end;

function TSecurePageControl.DoUndockClientMsg(NewTarget, Client: TControl): boolean;
begin
  FPageToUndock := FindPageWithDockClient(Client);
  Result := inherited DoUndockClientMsg(NewTarget, Client);
end;

function TSecurePageControl.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result:=(ChildClass<>nil) and (ChildClass.InheritsFrom(PageClass));
  if Widgetset.GetLCLCapability(lcAllowChildControlsInNativeControls) = LCL_CAPABILITY_YES then Result := True;
end;

function TSecurePageControl.FindNextPage(CurPage: TSecureTabSheet; GoForward,
  CheckTabVisible: Boolean): TSecureTabSheet;
var
  I, StartIndex: Integer;
begin
  Result := nil;
  if PageCount = 0 then
    exit;
  StartIndex := IndexOf(CurPage);
  if StartIndex < 0 then
    if GoForward then
      StartIndex := PageCount - 1
    else
      StartIndex := 0;
  i := StartIndex;
  repeat
    if GoForward then
    begin
      Inc(i);
      if i = PageCount then
        i := 0;
    end else
    begin
      if i = 0 then
        i := PageCount;
      Dec(I);
    end;
    if not CheckTabVisible or Pages[i].TabVisible then
    begin
      Result := Pages[i];
      exit;
    end;
  until i = StartIndex;
end;

procedure TSecurePageControl.SelectNextPage(GoForward: Boolean);
begin
  SelectNextPage(GoForward,true);
end;

procedure TSecurePageControl.SelectNextPage(GoForward: Boolean;
  CheckTabVisible: Boolean);
var
  NextPage: TSecureTabSheet;
begin
  NextPage:=FindNextPage(ActivePage,GoForward,CheckTabVisible);
  if NextPage<>nil then ActivePage:=NextPage;
end;

// Convenience routine, to make the TSecurePageControl more intuitive to use
// A Lazarus addition
function TSecurePageControl.AddTabSheet: TSecureTabSheet;
begin
  Result := TSecureTabSheet.Create(Self);
  Result.PageControl := TPageControl(Self);
end;





end.

