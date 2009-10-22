program HyperCos_v1;

uses
  Forms,
  formMain in 'formMain.pas' {frmMain},
  SynchedThreads in 'SynchedThreads.pas',
  uFirmata in 'uFirmata.pas',
  uScratch in 'uScratch.pas',
  uHyperCos in 'uHyperCos.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);

  // Rather than requiring the form to know details about the
  // class that implements the HyperCos functionality, we use
  // interfaces to hide implementation details. The form need
  // only implement IHyperCosDisplay and handle IHyperCosControl
  // methods.
  // Since the form is a display and control interface, it is
  // not appropriate for it to do the work of creating the
  // HyperCos instance that will be used. Instead an Inversion of
  // Control technique called dependency injection is used to
  // provide the form instance with the required interface to
  // HyperCos. The THyperCosFactory is used to build the instance
  // HyperCos and to inject it's dependency, an IHyperCosDisplay
  // interface, in this case provided by the frmMain instance.
  // It is important to note that HyperCos doesn't know anything
  // about the display form, all it knows is that it has a Display.
  //
  // In this example frmMain impliments the IHyperCosDisplay interface
  // so we pass the frmMain instance in this GetHyperCos call. If we
  // had a different object that implimented IHyperCosDisplay, maybe
  // a form called frmDisplay, then we could pass that instead, while
  // still assigning the HyperCos instance to frmMain.HyperCos.
  frmMain.HyperCos := THyperCosFactory.GetHyperCos( frmMain, 'localhost' );

  Application.Run;
end.
