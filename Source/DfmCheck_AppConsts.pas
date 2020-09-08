{******************************************************************************}
{*                                                                            *}
{* DfmCheck                                                                   *}
{*                                                                            *}
{* (C) 2006 Andreas Hausladen                                                 *}
{*                                                                            *}
{******************************************************************************}

unit DfmCheck_AppConsts;

{$I DfmCheck.inc}

interface

const
  sVersion = '1.6';

resourcestring
  { Options }
  RsOptionDoesNotExist = 'Option "%s" does not exist';

  { Utils }
  RsInvalidDfmFile = '"%s" is no valid DFM file';

  { Progress }
  RsFinished = 'Finished.';

  { Main }
  RsConfigurePlugin = 'Delphi DFM Check Options...';
  RsRunDfmCheck = 'Check DFM files';
  RsOpenCloseAllForms = 'Open/Close all forms';
  RsConfirmSaveModifiedFile = 'File "%s" has changed. Do you want to save the changes?';

  RsGerConfigurePlugin = 'Delphi DFM Check Optionen...';
  RsGerRunDfmCheck = 'DFM Dateien überprüfen';
  RsGerOpenCloseAllForms = 'Öffnen/Schließen aller Formulare';
  RsGerConfirmSaveModifiedFile = 'Datei "%s" wurde verändert. Sollen die Änderungen gespeichert werden?';

var
  sConfirmSaveModifiedFile: string;

implementation

end.

