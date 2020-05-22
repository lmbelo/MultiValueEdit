unit MultiValueEdit;

interface

uses
  Buttons, StdCtrls, Contnrs, Controls, Messages, Windows, Graphics, Classes;

type
  TMultiValueButtom = class(Buttons.TSpeedButton)
  private
    const BTN_CLOSE_GLYPH: string = 'X';
  private
    procedure CalcButtonLayout(ACanvas: TCanvas; const AClient: TRect;
      const AOffset: TPoint; const ACaption: string; ALayout: TButtonLayout;
      AMargin, ASpacing: Integer; var ASeparatorPos: TPoint;
      var ATextBounds: TRect; var AGlyphBounds: TRect; ABiDiFlags: Longint);
    procedure DrawButtonGlyph(ACanvas: TCanvas; var AGlyphBounds: TRect;
      AState: TButtonState; ABiDiFlags: Longint);
    procedure DrawButtonText(ACanvas: TCanvas; const ACaption: string;
      ATextBounds: TRect; AState: TButtonState; ABiDiFlags: Longint);
    procedure DrawGlyphSeparator(ACanvas: TCanvas; const ASeparatorPos: TPoint;
      const AClient: TRect);
    function Draw(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      const ACaption: string; ALayout: TButtonLayout; AMargin, ASpacing: Integer;
      AState: TButtonState; ATransparent: Boolean; ABiDiFlags: Longint): TRect;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Click; override;
  end;

  TCustomMultiValueEdit = class(StdCtrls.TCustomEdit)
  private
    const CTRL_SPACE = 2;
    const MIN_INPUT_WIDTH = 50;
  private
    FChildren: TObjectList;
    FDelimiter: char;
    FInputTextAligned: boolean;

    function GetDelimitedText: string;
    procedure SetDelimitedText(const Value: string);  

    procedure AddControl(const AText: string);
    function AddControls(const AText: string): integer;
    procedure RemoveControl(const AControl: TWinControl);
    procedure RemoveControls();
    function ProcessText(): boolean;

    function GetTotalControlsWidth(): integer;
    function CalcTotalControlsWidth(): integer;

    procedure BoundControls(const APrepareForInput: boolean = true);
    procedure SetCaretPosition(const ADirection: TLeftRight);
    procedure CalcSelRect();
    function CanScrollControls(const ADirection: TLeftRight): boolean;
    function CalcScrollBy(const ADirection: TLeftRight; const ADefaultScrollBy: integer = 5): integer;
    procedure ScrollControls(const ADirection: TLeftRight; AScrollBy: integer = 1);
    function GetSelWidth(): integer;
    function ShouldScrollToLeftForTextChange(var AScrollBy: integer): boolean;
    function ShouldScrollToRightForTextChange(var AScrollBy: integer): boolean;
    function CalcTextWidth(const AText: string): integer;
    procedure OnBtnClick(Sender: TObject);
  private
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  protected
    function IsValidChar(const AKey: Char): boolean; virtual;
    procedure Paint(); virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure Change; override;
    procedure Loaded; override;

    function GetMinLeft(): integer; virtual;
    function GetMaxLeft(): integer; virtual;
  protected
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd(); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;

    property Delimiter: char read FDelimiter write FDelimiter default #13;
    property DelimitedText: string read GetDelimitedText write SetDelimitedText;
    property InputTextAligned: boolean read FInputTextAligned write FInputTextAligned default true;
  end;

  TMultiValueEdit = class(TCustomMultiValueEdit)
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property Delimiter;
    property DelimitedText;
    property InputTextAligned; 
  end;

procedure Register;

implementation

uses
  SysUtils, Themes, Math;

{ TMultiValueButtom }

procedure TMultiValueButtom.Click;
begin
  inherited;
end;

constructor TMultiValueButtom.Create(AOwner: TComponent);
begin
  inherited;
  Cursor := crArrow;
end;

procedure TMultiValueButtom.CalcButtonLayout(ACanvas: TCanvas;
  const AClient: TRect; const AOffset: TPoint; const ACaption: string;
  ALayout: TButtonLayout; AMargin, ASpacing: Integer; var ASeparatorPos: TPoint;
  var ATextBounds: TRect; var AGlyphBounds: TRect; ABiDiFlags: Integer);
var
  TextPos: TPoint;
  GlyphPos: TPoint;
  ClientSize, GlyphSize, TextSize: TPoint;
  TotalSize: TPoint;
  SeparatorSize: TPoint;
begin
  if (ABiDiFlags and DT_RIGHT) = DT_RIGHT then
    if ALayout = blGlyphLeft then
      ALayout := blGlyphRight
    else 
      if ALayout = blGlyphRight then
        ALayout := blGlyphLeft;

  { calculate the item sizes }
  ClientSize := Point(AClient.Right - AClient.Left, AClient.Bottom - AClient.Top);
  if Length(ACaption) > 0 then begin
    ATextBounds := Rect(0, 0, AClient.Right - AClient.Left, 0);
    DrawText(ACanvas.Handle, PChar(ACaption), Length(ACaption), ATextBounds, DT_CALCRECT or ABiDiFlags);
    TextSize := Point(ATextBounds.Right - ATextBounds.Left, ATextBounds.Bottom - ATextBounds.Top);

    AGlyphBounds := Rect(0, 0, AClient.Right - AClient.Left, 0);
    DrawText(ACanvas.Handle, PChar(BTN_CLOSE_GLYPH), Length(BTN_CLOSE_GLYPH), AGlyphBounds, DT_CALCRECT or ABiDiFlags);
    GlyphSize := Point(AGlyphBounds.Right - AGlyphBounds.Left, AGlyphBounds.Bottom - AGlyphBounds.Top);
  end else begin
    ATextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0,0);
  end;

  if ALayout in [blGlyphLeft, blGlyphRight] then begin
    SeparatorSize := Point(1, ClientSize.Y - 1);
  end else begin
    SeparatorSize := Point(ClientSize.X, 1 - 1);
  end;
    
  { If the layout has the glyph on the right or the left, then both the
    text and the glyph are centered vertically.  If the glyph is on the top
    or the bottom, then both the text and the glyph are centered horizontally.}
  if ALayout in [blGlyphLeft, blGlyphRight] then begin
    GlyphPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
    ASeparatorPos.Y := (ClientSize.Y - SeparatorSize.Y + 1) div 2;
  end else begin
    GlyphPos.X := (ClientSize.X - TextSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
    ASeparatorPos.X := (ClientSize.X - SeparatorSize.X + 1) div 2;
  end;
    
  { if there is no text or no Glyph, then Spacing is irrelevant }
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    ASpacing := 0;
    
  { adjust Margin and Spacing }
  if AMargin = -1 then begin
    if ASpacing < 0 then begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if ALayout in [blGlyphLeft, blGlyphRight] then
        AMargin := (ClientSize.X - TotalSize.X) div 3
      else
        AMargin := (ClientSize.Y - TotalSize.Y) div 3;
      ASpacing := AMargin;
    end else begin
      TotalSize := Point(GlyphSize.X + ASpacing + TextSize.X, GlyphSize.Y +
        ASpacing + TextSize.Y);
      if ALayout in [blGlyphLeft, blGlyphRight] then
        AMargin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        AMargin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end else begin
    if ASpacing < 0 then begin
      TotalSize := Point(ClientSize.X - (AMargin + GlyphSize.X), ClientSize.Y - (AMargin + GlyphSize.Y));
      if ALayout in [blGlyphLeft, blGlyphRight] then
        ASpacing := (TotalSize.X - TextSize.X) div 2
      else
        ASpacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;
    
  case ALayout of
    blGlyphLeft:
      begin
        GlyphPos.X := AMargin;
        ASeparatorPos.X := GlyphPos.X + GlyphSize.X + ASpacing;
        TextPos.X := GlyphPos.X + GlyphSize.X + SeparatorSize.X + ASpacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - AMargin - GlyphSize.X;
        ASeparatorPos.X := GlyphPos.X - ASpacing - SeparatorSize.X;
        TextPos.X := ASeparatorPos.X - ASpacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := AMargin;
        ASeparatorPos.X := GlyphPos.Y + GlyphSize.Y + ASpacing;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + SeparatorSize.Y + ASpacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - AMargin - GlyphSize.Y;
        ASeparatorPos.Y := GlyphPos.Y - ASpacing - SeparatorSize.Y;
        TextPos.Y := ASeparatorPos.Y - ASpacing - TextSize.Y;
      end;
  end;

  { Themed text is not shifted, but gets a different color. }
  if ThemeServices.ThemesEnabled then begin
    OffsetRect(ATextBounds, TextPos.X + AClient.Left, TextPos.Y + AClient.Top);
    OffsetRect(AGlyphBounds, GlyphPos.X + AClient.Left, GlyphPos.Y + AClient.Top);
  end else begin
    OffsetRect(ATextBounds, TextPos.X + AClient.Left + AOffset.X, TextPos.Y + AClient.Top + AOffset.Y);
    OffsetRect(AGlyphBounds, GlyphPos.X + AClient.Left + AOffset.X, GlyphPos.Y + AClient.Top + AOffset.Y);
  end;
end;

procedure TMultiValueButtom.DrawButtonGlyph(ACanvas: TCanvas;
  var AGlyphBounds: TRect; AState: TButtonState; ABiDiFlags: Longint);
var
  Details: TThemedElementDetails;
  R: TRect;
  Button: TThemedButton;
begin
  with AGlyphBounds do begin
    if ThemeServices.ThemesEnabled then begin
      R.TopLeft := Point(AGlyphBounds.Top, AGlyphBounds.Left);
      R.Right := R.Left + 8;
      R.Bottom := R.Top + 8;
      case AState of
        bsDisabled:
          Button := tbPushButtonDisabled;
        bsDown,
        bsExclusive:
          Button := tbPushButtonPressed;
      else
        // bsUp
        Button := tbPushButtonNormal;
      end;
      Details := ThemeServices.GetElementDetails(Button);
      DrawText(ACanvas.Handle, PChar(BTN_CLOSE_GLYPH), Length(BTN_CLOSE_GLYPH), AGlyphBounds, DT_CENTER or DT_VCENTER or ABiDiFlags);
    end else
      if Transparent or (AState = bsExclusive) then begin
        DrawText(ACanvas.Handle, PChar(BTN_CLOSE_GLYPH), Length(BTN_CLOSE_GLYPH), AGlyphBounds, DT_CENTER or DT_VCENTER or ABiDiFlags);
      end else
        DrawText(ACanvas.Handle, PChar(BTN_CLOSE_GLYPH), Length(BTN_CLOSE_GLYPH), AGlyphBounds, DT_CENTER or DT_VCENTER or ABiDiFlags);
  end;
end;

procedure TMultiValueButtom.DrawButtonText(ACanvas: TCanvas;
  const ACaption: string; ATextBounds: TRect; AState: TButtonState;
  ABiDiFlags: Integer);
begin
  with ACanvas do begin
    Brush.Style := bsClear;
    if AState = bsDisabled then begin
      OffsetRect(ATextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(ACaption), Length(ACaption), ATextBounds, DT_CENTER or DT_VCENTER or ABiDiFlags);
      OffsetRect(ATextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(ACaption), Length(ACaption), ATextBounds, DT_CENTER or DT_VCENTER or ABiDiFlags);
    end else
      DrawText(Handle, PChar(ACaption), Length(ACaption), ATextBounds, DT_CENTER or DT_VCENTER or ABiDiFlags);
  end;
end;

procedure TMultiValueButtom.DrawGlyphSeparator(ACanvas: TCanvas;
  const ASeparatorPos: TPoint; const AClient: TRect);
begin
  ACanvas.MoveTo(ASeparatorPos.X, AClient.Top);
  ACanvas.LineTo(ASeparatorPos.X, AClient.Bottom - AClient.Top + 2);
end;

function TMultiValueButtom.Draw(ACanvas: TCanvas; const AClient: TRect;
  const AOffset: TPoint; const ACaption: string; ALayout: TButtonLayout; AMargin,
  ASpacing: Integer; AState: TButtonState; ATransparent: Boolean;
  ABiDiFlags: Integer): TRect;
var
  SeparatorPos: TPoint;
  TextBounds: TRect;
  GlyphBounds: TRect;
begin
  CalcButtonLayout(ACanvas, AClient, AOffset, ACaption, ALayout, AMargin, ASpacing, SeparatorPos, TextBounds, GlyphBounds, ABiDiFlags);
  DrawButtonGlyph(ACanvas, GlyphBounds, AState, ABiDiFlags);
  DrawGlyphSeparator(ACanvas, SeparatorPos, AClient);
  DrawButtonText(ACanvas, ACaption, TextBounds, AState, ABiDiFlags);
end;

procedure TMultiValueButtom.Paint;
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
begin
  if not Enabled then begin
    FState := bsDisabled;
  end else if FState = bsDisabled then
    if Down and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  Canvas.Font := Self.Font;

  if ThemeServices.ThemesEnabled then begin
    PerformEraseBackground(Self, Canvas.Handle);

    if not Enabled then
      Button := tbPushButtonDisabled
    else
      if FState in [bsDown, bsExclusive] then
        Button := tbPushButtonPressed
      else
        if MouseInControl then
          Button := tbPushButtonHot
        else
          Button := tbPushButtonNormal;

    ToolButton := ttbToolbarDontCare;
    if Flat then begin
      case Button of
        tbPushButtonDisabled:
          Toolbutton := ttbButtonDisabled;
        tbPushButtonPressed:
          Toolbutton := ttbButtonPressed;
        tbPushButtonHot:
          Toolbutton := ttbButtonHot;
        tbPushButtonNormal:
          Toolbutton := ttbButtonNormal;
      end;
    end;

    PaintRect := ClientRect;
    if ToolButton = ttbToolbarDontCare then begin
      Details := ThemeServices.GetElementDetails(Button);
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);
    end else begin
      Details := ThemeServices.GetElementDetails(ToolButton);
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect);
      PaintRect := ThemeServices.ContentRect(Canvas.Handle, Details, PaintRect);
    end;

    if Button = tbPushButtonPressed then begin
      // A pressed speed button has a white text. This applies however only to flat buttons.
      if ToolButton <> ttbToolbarDontCare then
        Canvas.Font.Color := clHighlightText;
      Offset := Point(1, 0);
    end else
      Offset := Point(0, 0);
    Draw(Canvas, PaintRect, Offset, Caption, Layout, Margin, Spacing, FState, Transparent, DrawTextBiDiModeFlags(0));
  end else begin
    PaintRect := Rect(0, 0, Width, Height);
    if not Flat then begin
      DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if FState in [bsDown, bsExclusive] then
        DrawFlags := DrawFlags or DFCS_PUSHED;
      DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
    end else begin
      if (FState in [bsDown, bsExclusive]) or
        (MouseInControl and (FState <> bsDisabled)) or
        (csDesigning in ComponentState) then
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]],
          FillStyles[Transparent] or BF_RECT)
      else if not Transparent then begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(PaintRect);
      end;
      InflateRect(PaintRect, -1, -1);
    end;
    if FState in [bsDown, bsExclusive] then begin
      if (FState = bsExclusive) and (not Flat or not MouseInControl) then begin
        Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
        Canvas.FillRect(PaintRect);
      end;
      Offset.X := 1;
      Offset.Y := 1;
    end else begin
      Offset.X := 0;
      Offset.Y := 0;
    end;
    Draw(Canvas, PaintRect, Offset, Caption, Layout, Margin, Spacing, FState, Transparent, DrawTextBiDiModeFlags(0));
  end;
end;

{ TMultiValueEdit }

constructor TCustomMultiValueEdit.Create(AOwner: TComponent);
begin
  inherited;
  FDelimiter := #13;
  FInputTextAligned := true;
  ControlStyle := ControlStyle - [csSetCaption];
  FChildren := TObjectList.Create(false);
end;

destructor TCustomMultiValueEdit.Destroy;
begin
  FChildren.Destroy();
  inherited;                    
end;

function TCustomMultiValueEdit.GetDelimitedText: string;
var
  I: Integer;
  LControl: TWinControl;
  LLen: Integer;
  LText: PChar;
begin
  for I := 0 to FChildren.Count - 1 do begin
    LControl := TWinControl(FChildren[I]);
    LLen := LControl.GetTextLen();
    if LLen > 0 then begin
      LText := StrAlloc(LControl.GetTextLen() + 1);
      try
        LControl.GetTextBuf(LText, StrBufSize(LText));
        Result := Result + LText + FDelimiter;
      finally
        StrDispose(LText);
      end;
    end;
  end;
end;

function TCustomMultiValueEdit.GetMaxLeft: integer;
begin
  Result := ClientWidth - MIN_INPUT_WIDTH;
end;

function TCustomMultiValueEdit.GetMinLeft: integer;
begin
  Result := 1;
end;

function TCustomMultiValueEdit.GetSelWidth: integer;
var
  LLoc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@LLoc));
  Result := (LLoc.Right - LLoc.Left);
end;

function TCustomMultiValueEdit.CalcScrollBy(const ADirection: TLeftRight; const ADefaultScrollBy: integer): integer;
var
  LControl: TWinControl;
begin
  Result := ADefaultScrollBy;
  if ADirection = taRightJustify then begin
    if FChildren.Count > 0 then begin
      LControl := TWinControl(FChildren[0]);
      if (LControl.Left >= GetMinLeft()) then
        Result := 0;

      if (LControl.Left + ADefaultScrollBy >= GetMinLeft()) then
        Result := GetMinLeft() + Abs(LControl.Left);
    end;
  end else if ADirection = taLeftJustify then begin
    if FChildren.Count > 0 then begin
      LControl := TWinControl(FChildren[FChildren.Count - 1]);
      if (LControl.Left + LControl.Width + CTRL_SPACE <= GetMaxLeft()) then
        Result := 0;

      if (LControl.Left + LControl.Width + CTRL_SPACE - ADefaultScrollBy <= GetMaxLeft()) then
        Result := (LControl.Left + LControl.Width + CTRL_SPACE) - GetMaxLeft();
    end;
  end;
end;

function TCustomMultiValueEdit.CalcTextWidth(const AText: string): integer;
var
  LCanvas: TControlCanvas;
begin
  LCanvas := TControlCanvas.Create();
  try
    LCanvas.Control := Self;
    LCanvas.Font.Assign(Font);
    Result := LCanvas.TextWidth(AText);
  finally
    LCanvas.Free();
  end;
end;

function TCustomMultiValueEdit.CalcTotalControlsWidth: integer;
begin
  Result := GetTotalControlsWidth() + (FChildren.Count * CTRL_SPACE)
end;

function TCustomMultiValueEdit.CanScrollControls(
  const ADirection: TLeftRight): boolean;
var
  LLeft: Integer;
  LWidth: Integer;
begin
  Result := FChildren.Count > 0;
  if (Result) then begin
    if ADirection = taRightJustify then begin
      LLeft := TWinControl(FChildren[0]).Left;
      Result := LLeft <= GetMinLeft();
    end else if ADirection = taLeftJustify then begin
      LLeft := TWinControl(FChildren[FChildren.Count - 1]).Left;
      LWidth := TWinControl(FChildren[FChildren.Count - 1]).Width;
      Result := LLeft + LWidth + CTRL_SPACE >= GetMaxLeft();
    end;
  end;
  if (FInputTextAligned) then
    Result := Result and (Length(Text) = 0);
end;

function TCustomMultiValueEdit.GetTotalControlsWidth: integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FChildren.Count - 1 do begin
    Result := Result + TWinControl(FChildren[I]).Width;
  end;
end;

function TCustomMultiValueEdit.IsValidChar(const AKey: Char): boolean;
begin
  Result := true;
end;

procedure TCustomMultiValueEdit.CalcSelRect();
var
  LLoc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LongInt(@LLoc));
  LLoc.Bottom := ClientHeight - 2;
  LLoc.Top := 1;
  LLoc.Right := ClientWidth - 1;
  if FInputTextAligned then begin
    if (FChildren.Count > 0) then
      LLoc.Left := (TWinControl(FChildren[FChildren.Count - 1]).Left
                  + TWinControl(FChildren[FChildren.Count - 1]).Width
                  + CTRL_SPACE)
    else
      LLoc.Left := 1;
  end else begin
    LLoc.Left := 1;
  end;
  SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@LLoc));
end;

function TCustomMultiValueEdit.AddControls(const AText: string): integer;
var
  LStrings: TStrings;
  I: Integer;
begin
  Result := 0;
  LStrings := TStringList.Create();
  try
    if (Pos(FDelimiter, AText) > 0) and (ExtractStrings([FDelimiter], [], PChar(AText), LStrings) > 0) then begin
      for I := 0 to LStrings.Count - 1 do begin
        AddControl(LStrings[I]);
      end;
      Result := LStrings.Count;
    end;
  finally
    LStrings.Free();
  end;
end;

procedure TCustomMultiValueEdit.BoundControls(const APrepareForInput: boolean);
var
  MinHeight: Integer;
  LBtnHeight: integer;
  I: Integer;
  LButton: TWinControl;
  LLeft: Integer;    
begin
  MinHeight := 5;

  { text edit bug: if size to less than minheight,
    then edit ctrl does not display the text }

  if Height < MinHeight then
    Height := MinHeight;

  LBtnHeight := Height;

  LLeft := CalcTotalControlsWidth();
  if (LLeft > GetMaxLeft()) then
    LLeft := - (LLeft - GetMaxLeft())
  else
    LLeft := GetMinLeft();

  if FChildren.Count > 0 then begin
    for I := 0 to FChildren.Count - 1 do begin
      LButton := TWinControl(FChildren[I]);
      if NewStyleControls and Ctl3D then begin
        LButton.SetBounds(LLeft, 1, LButton.Width, LBtnHeight - 5)
      end else begin
        LButton.SetBounds(LLeft, 1, LButton.Width, LBtnHeight - 1);
      end;
      LLeft := LLeft + LButton.Width + CTRL_SPACE;
    end;
  end;

  if APrepareForInput then begin
    CalcSelRect();
    SetCaretPosition(taLeftJustify);
  end;
end;

procedure TCustomMultiValueEdit.ScrollControls(const ADirection: TLeftRight; AScrollBy: integer);
var
  I: Integer;
  LButton: TWinControl;
  LLeft: Integer;
begin
  if (AScrollBy = 0) then Exit;

  for I := 0 to FChildren.Count - 1 do begin
    LButton := TWinControl(FChildren[I]);

    LLeft := LButton.Left;
    if ADirection = taRightJustify then begin
      LLeft := LButton.Left + AScrollBy
    end else if ADirection = taLeftJustify then
      LLeft := LButton.Left - AScrollBy;

    LButton.SetBounds(LLeft, LButton.Top, LButton.Width, LButton.Height);
  end;
end;

procedure TCustomMultiValueEdit.SetCaretPosition(const ADirection: TLeftRight);
begin
  SendMessage(Handle, EM_SETSEL, 1, 0); //For weird windows bug corretion involving selection rectangle and caret
  if ADirection = taRightJustify then begin
    SendMessage(Handle, EM_SETSEL, Length(Text), Length(Text));
  end else if ADirection = taLeftJustify then begin
    SendMessage(Handle, EM_SETSEL, 0, 0);
  end;
end;

procedure TCustomMultiValueEdit.SetDelimitedText(const Value: string);
begin
  RemoveControls();
  AddControls(Value);
end;

function TCustomMultiValueEdit.ShouldScrollToLeftForTextChange(var AScrollBy: integer): boolean;
var
  LTextWidth: Integer;
  LSelWidth: Integer;
begin
  if (FChildren.Count > 0) and (FInputTextAligned) then begin
    LTextWidth := CalcTextWidth(Text);
    LSelWidth := GetSelWidth();
    Result := LTextWidth > LSelWidth;
    if Result then
      AScrollBy := (LTextWidth - LSelWidth);

  end else Result := false;
end;

function TCustomMultiValueEdit.ShouldScrollToRightForTextChange(var AScrollBy: integer): boolean;
var
  LTextWidth: Integer;
  LFirstControl: TWinControl;
  LSelWidth: Integer;
begin
  if (FChildren.Count > 0) and (FInputTextAligned) then begin
    LFirstControl := TWinControl(FChildren[0]);
    LTextWidth := Max(MIN_INPUT_WIDTH, CalcTextWidth(Text));
    LSelWidth := GetSelWidth();
    Result := (LFirstControl.Left < GetMinLeft()) and (LTextWidth < LSelWidth);
    if Result then
      AScrollBy := (LSelWidth - LTextWidth);

  end else Result := false;
end;

procedure TCustomMultiValueEdit.Paint;
begin
//
end;

procedure TCustomMultiValueEdit.PaintWindow(DC: HDC);
begin
  Paint();
end;

function TCustomMultiValueEdit.ProcessText(): boolean;
begin
  Result := AddControls(Text) > 0;
end;

procedure TCustomMultiValueEdit.OnBtnClick(Sender: TObject);
begin
  RemoveControl(TWinControl(Sender));
end;

procedure TCustomMultiValueEdit.AddControl(const AText: string);
var
  LControl: TMultiValueButtom;
begin
  LControl := TMultiValueButtom.Create(Self);
  LControl.Parent := Self;
  LControl.Flat := false;
  LControl.Caption := AText;
  LControl.Width := LControl.Canvas.TextWidth(LControl.Caption) + 25 + LControl.Margin;
  LControl.Visible := true;
  LControl.Layout := blGlyphRight;
  LControl.OnClick := OnBtnClick;
  FChildren.Add(LControl);
  BoundControls();
end;

procedure TCustomMultiValueEdit.RemoveControl(const AControl: TWinControl);
begin
  FChildren.Remove(AControl);
  AControl.Destroy;
  BoundControls();
end;

procedure TCustomMultiValueEdit.RemoveControls;
var
  I: Integer;
begin
  for I := FChildren.Count - 1 downto 0 do begin
    RemoveControl(TWinControl(FChildren[I]));
  end;
end;

procedure TCustomMultiValueEdit.Change;
var
  LScrollBy: Integer;
begin
  inherited;
  if ProcessText() then
    Clear();

  if ShouldScrollToLeftForTextChange(LScrollBy) then begin
    ScrollControls(taLeftJustify, LScrollBy);
  end else if ShouldScrollToRightForTextChange(LScrollBy) then begin
    ScrollControls(taRightJustify, LScrollBy);
  end;
  CalcSelRect();
  SetCaretPosition(taRightJustify);
end;

procedure TCustomMultiValueEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

procedure TCustomMultiValueEdit.CreateWnd;
begin
  inherited CreateWnd;
  BoundControls();
end;

procedure TCustomMultiValueEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_LEFT) then begin
    if CanScrollControls(taRightJustify) then begin
      ScrollControls(taRightJustify, CalcScrollBy(taRightJustify))
    end;
  end else if (Key = VK_RIGHT) then begin
    if CanScrollControls(taLeftJustify) then begin
      ScrollControls(taLeftJustify, CalcScrollBy(taLeftJustify));
    end;
  end else if (Key = VK_HOME) then begin
    if CanScrollControls(taRightJustify) then begin
      ScrollControls(taRightJustify, CalcScrollBy(taRightJustify, High(integer)));
    end;
  end else if (Key = VK_END) then begin
    if CanScrollControls(taLeftJustify) then begin
      ScrollControls(taLeftJustify, CalcScrollBy(taLeftJustify, High(integer)));
    end;
  end;

  inherited KeyDown(Key, Shift);
end;

procedure TCustomMultiValueEdit.KeyPress(var Key: Char);
begin 
  if not IsValidChar(Key) then begin
    Key := #0;
    MessageBeep(0)
  end;

  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TCustomMultiValueEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
end;

procedure TCustomMultiValueEdit.Loaded;
begin
  inherited;
  AutoSize := false;
end;

procedure TCustomMultiValueEdit.CMEnter(var Message: TCMGotFocus);
begin
  BoundControls();
  
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;    
  inherited;
end;

procedure TCustomMultiValueEdit.CMExit(var Message: TCMExit);
begin
  BoundControls();
end;

procedure TCustomMultiValueEdit.WMCut(var Message: TWMCut);
begin
  if ReadOnly then Exit;
  inherited;
end;

procedure TCustomMultiValueEdit.WMPaint(var Message: TWMPaint);
begin
  ControlState := ControlState + [csCustomPaint];
  inherited;
  ControlState := ControlState - [csCustomPaint];
end;

procedure TCustomMultiValueEdit.WMPaste(var Message: TWMPaste);
begin
  if ReadOnly then Exit;
  inherited;
end;

procedure TCustomMultiValueEdit.WMSize(var Message: TWMSize);
begin
  inherited;
  if (Width < MIN_INPUT_WIDTH + 50) then
    Width := MIN_INPUT_WIDTH + 50;
  BoundControls();
end;

procedure Register;
begin
  RegisterComponents('LMBTec', [TMultiValueEdit]);
end;

end.
