{

FPDF Pascal Extensions

Based on the library FPDF written in PHP by Olivier PLATHEY and
         Free JPDF Pascal from Jean Patrick e Gilson Nunes

 Copyright (C) 2023 Projeto ACBr - Daniel Simões de Almeida

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files (the "Software"), to deal in
 the Software without restriction, including without limitation the rights to
 use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
 of the Software, and to permit persons to whom the Software is furnished to do
 so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.

 Except as contained in this notice, the name of <Projeto ACBr> shall not be
 used in advertising or otherwise to promote the sale, use or other dealings in
 this Software without prior written authorization from <Projeto ACBr>.

}

unit fpdf_ext;

// Define USESYNAPSE if you want to force use of synapse
{.$DEFINE USESYNAPSE}

// If you don't want the AnsiString vs String warnings to bother you
{.$DEFINE REMOVE_CAST_WARN}

{$IfNDef FPC}
  {$Define USESYNAPSE}

  {$IFDEF REMOVE_CAST_WARN}
    {$WARN IMPLICIT_STRING_CAST OFF}
    {$WARN IMPLICIT_STRING_CAST_LOSS OFF}
  {$ENDIF}
{$EndIf}

{$IfDef FPC}
  {$Mode objfpc}{$H+}
  {$Define USE_UTF8}
{$EndIf}

{$IfDef POSIX}
  {$IfDef LINUX}
    {$Define USE_UTF8}
  {$EndIf}
  {$Define FMX}
{$EndIf}

{$IfDef NEXTGEN}
  {$ZEROBASEDSTRINGS OFF}
  {$Define USE_UTF8}
{$EndIf}

interface

uses
  Classes, SysUtils,
  fpdf,
  {$IFDEF USESYNAPSE}
   httpsend, ssl_openssl
  {$ELSE}
   fphttpclient, opensslsockets
  {$ENDIF};

{$IfDef NEXTGEN}
type
  AnsiString = RawByteString;
  AnsiChar = UTF8Char;
  PAnsiChar = MarshaledAString;
  PPAnsiChar = ^MarshaledAString;
  WideString = String;
{$EndIf}

type

  { TFPDFExt }

  TFPDFExt = class(TFPDF)
  private
    FProxyHost: String;
    FProxyPass: String;
    FProxyPort: String;
    FProxyUser: String;

    procedure GetImageFromURL(const aURL: String; const aResponse: TStream);
  protected

  public
    constructor Create; overload;

    procedure Image(const vFileOrURL: String; vX: Double = -9999; vY: Double = -9999;
      vWidth: Double = 0; vHeight: Double = 0; const vLink: String = ''); overload; override;

    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: String read FProxyPort write FProxyPort;
    property ProxyUser: String read FProxyUser write FProxyUser;
    property ProxyPass: String read FProxyPass write FProxyPass;
  end;

implementation

constructor TFPDFExt.Create;
begin
  inherited Create;
  FProxyHost := '';
  FProxyPass := '';
  FProxyPort := '';
  FProxyUser := '';
end;

procedure TFPDFExt.Image(const vFileOrURL: String; vX: Double; vY: Double;
  vWidth: Double; vHeight: Double; const vLink: String);
var
  s, ext: String;
  ms: TMemoryStream;
  img: TFPDFImageInfo;
  i: Integer;
begin
  //Put an image From Web on the page
  s := LowerCase(vFileOrURL);
  if ((Pos('http://', s) > 0) or (Pos('https://', s) > 0)) then
  begin
    img := FindUsedImage(vFileOrURL);
    if (img.data = '') then
    begin
      ext := '';
      if (Pos('.jpg', s) > 0) or (Pos('.jpeg', s) > 0) then
        ext := 'JPG'
      else if (Pos('.png', s) > 0) then
        ext := 'PNG'
      else
        Error('Supported image not found in URL: ' + vFileOrURL);

      ms := TMemoryStream.Create;
      try
        GetImageFromURL(vFileOrURL, ms);
        inherited Image(ms, ext, vX, vY, vWidth, vHeight, vLink);
        i := Length(Self.images)-1;
        Self.images[i].ImageName := vFileOrURL;
      finally
         ms.Free;
      end;
    end
    else
      inherited Image(img, vX, vY, vWidth, vHeight, vLink);
  end
  else
    inherited Image(vFileOrURL, vX, vY, vWidth, vHeight, vLink);
end;

{$IfDef USESYNAPSE}
procedure TFPDFExt.GetImageFromURL(const aURL: String; const aResponse: TStream);
var
  vHTTP: THTTPSend;
  Ok: Boolean;
begin
  vHTTP := THTTPSend.Create;
  try
    if (ProxyHost <> '') then
    begin
      vHTTP.ProxyHost := ProxyHost;
      vHTTP.ProxyPort := ProxyPort;
      vHTTP.ProxyUser := ProxyUser;
      vHTTP.ProxyPass := ProxyPass;
    end;
    Ok := vHTTP.HTTPMethod('GET', aURL);
    if Ok then
    begin
      aResponse.Seek(0, soBeginning);
      aResponse.CopyFrom(vHTTP.Document, 0);
    end
    else
      Error('Http Error: '+IntToStr(vHTTP.ResultCode)+' when downloading from: '+aURL);
  finally
    vHTTP.Free;
  end;
end;

{$Else}
procedure TFPDFExt.GetImageFromURL(const aURL: String; const aResponse: TStream);
var
  vHTTP: TFPHTTPClient;
begin
  vHTTP := TFPHTTPClient.Create(nil);
  try
    if (ProxyHost <> '') then
    begin
      vHTTP.Proxy.Host := ProxyHost;
      vHTTP.Proxy.Port := StrToIntDef(ProxyPort, 0);
      vHTTP.Proxy.UserName := ProxyUser;
      vHTTP.Proxy.Password := ProxyPass;
    end;

    vHTTP.Get(aURL, aResponse);
  finally
    vHTTP.Free;
  end;
end;
{$EndIf}

end.
