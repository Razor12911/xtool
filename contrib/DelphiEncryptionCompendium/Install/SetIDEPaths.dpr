{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory
  of this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}
program SetIDEPaths;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Win.Registry,
  System.Classes;

var
  reg          : TRegistry;
  IDEKeys      : TStringList;
  PlatformKeys : TStringList;
  LibraryPaths : string;
  InstallDir   : string;

const
  IDERootKey       = 'SOFTWARE\Embarcadero\BDS';
  LibraryKey       = 'Library';
  LibraryPathValue = 'Search Path';
  SourceDir        = 'Source';

  /// <summary>
  ///   Determine installation directory relative to the path of this exe file
  /// </summary>
  function GetInstallPath: string;
  begin
    Result := ParamStr(0);
    Result := ExtractFilePath(result);
    Result := Result.TrimRight([System.SysUtils.PathDelim]);
    Result := Result.Remove(result.LastDelimiter(System.SysUtils.PathDelim));
    Result := Result + System.SysUtils.PathDelim + SourceDir;
  end;

  /// <summary>
  ///   Add the installation source directory to the library path of the currently
  ///   processed platform
  /// </summary>
  procedure AddInstallDirToLibraryPath;
  begin
    LibraryPaths := reg.ReadString(LibraryPathValue);
    // Only add path if not already added
    if pos(InstallDir.ToLower, LibraryPaths.ToLower) = 0 then
    begin
      if LibraryPaths.EndsWith(';') then
        LibraryPaths := LibraryPaths + InstallDir
      else
        LibraryPaths := LibraryPaths + ';' + InstallDir;

      reg.WriteString(LibraryPathValue, LibraryPaths);
      WriteLn('Success');
    end
    else
      WriteLn('Path exists already');
  end;

  /// <summary>
  ///   Add the library source path to all platforms of the currently processed
  ///   IDE version
  /// </summary>
  /// <param name="Platforms">
  ///   List of all platform library keys for the IDE version to be processed
  /// </param>
  /// <param name="IDEVersion">
  ///   IDE version of the IDE to be processed as part of a registry path
  /// </param>
  procedure ProcessAllPlatforms(Platforms: TStringList; IDEVersion: string);
  var
    platf : string;
  begin
    Assert(Assigned(Platforms), 'Empty list of platforms passed');
    Assert(IDEVersion <> '', 'Empty version number passed');

    for platf in Platforms do
    begin
      Write('Platform: ', platf:15, ' ');

      if reg.OpenKey(IDERootKey + '\' + IDEVersion + '\' + LibraryKey + '\' +
                     platf, false) then
      begin
        AddInstallDirToLibraryPath;
        reg.CloseKey;
      end
      else
        WriteLn('Failed (cannot open key)');
    end;
  end;

  /// <summary>
  ///   Process all RAD Studio IDEs found
  /// </summary>
  /// <param name="IDEVersions">
  ///   List of all IDE versions found
  /// </param>
  procedure ProcessAllIDEs(IDEVersions: TStringList);
  var
    IDEVersion: string;
  begin
    Assert(Assigned(IDEVersions), 'Empty list of IDE versions passed');


    for IDEVersion in IDEVersions do
    begin
      WriteLn;
      WriteLn('RADStudio: ' + IDEVersion);

      // Skip versions older than D2009
      if (StrToFloat(IDEVersion, TFormatSettings.Create('en-US'))  >= 6.0) then
      begin
        // Fetch all platforms for the currently processed IDE version
        if reg.OpenKey(IDERootKey + '\' + IDEVersion + '\' + LibraryKey, false) then
        begin
          PlatformKeys := TStringList.Create;
          try
            reg.GetKeyNames(PlatformKeys);
            reg.CloseKey;

            ProcessAllPlatforms(PlatformKeys, IDEVersion);
          finally
            PlatformKeys.Free;
          end;
        end
        else
          WriteLn('No platforms found');
      end
      else
        WriteLn('Versions prior to D2009 are not supported');
    end;
  end;

begin
  try
    WriteLn('Adding DEC library paths to RAD Studio');

    InstallDir := GetInstallPath;
    WriteLn('Path to add: ', InstallDir);

    // Fetch all RADStudio/Delphi/C++ Builder installations
    reg := TRegistry.Create;
    if reg.OpenKey(IDERootKey, false) then
    begin
      IDEKeys := TStringList.Create;
      try
        reg.GetKeyNames(IDEKeys);
        reg.CloseKey;

        if IDEKeys.Count > 0 then
          ProcessAllIDEs(IDEKeys)
        else
          WriteLn('No IDE versions found');
      finally
        IDEKeys.Free;
      end;
    end
    else
      WriteLn('No RAD Studio installation found');
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  WriteLn('');
  WriteLn('Press enter to quit and restart IDE');
  ReadLn;
end.
