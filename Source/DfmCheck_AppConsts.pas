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
  RsGerRunDfmCheck = 'DFM Dateien �berpr�fen';
  RsGerOpenCloseAllForms = '�ffnen/Schlie�en aller Formulare';
  RsGerConfirmSaveModifiedFile = 'Datei "%s" wurde ver�ndert. Sollen die �nderungen gespeichert werden?';

var
  sConfirmSaveModifiedFile: string;

implementation

end.

