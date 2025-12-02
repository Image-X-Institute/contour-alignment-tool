% Create target build options object, set build properties and build.
buildOpts = compiler.build.StandaloneApplicationOptions("R:\code\contour-alignment-tool\ContourAlignmentTool.mlapp");
buildOpts.AdditionalFiles = ["R:\code\contour-alignment-tool\ContourAlignmentTool_resources\ROI.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\The-X-small.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\The-X.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\about.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\alignment-directories.ai", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\alignment-directories.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\arrows.ai", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\auto.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\contour.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\contrast.ai", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\east.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\first.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\icon.ico", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\icon_16.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\icon_24.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\icon_32.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\icon_48.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\instruction-PatientPlanLoad.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\instructions-alignment.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\instructions-display.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\instructions-issue.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\instructions-loading.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\instructions-saving.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\instructions.ai", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\last.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\manual.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\mhaHeaderTemplate.mat", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\navigation.ai", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\next.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\north.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\northeast.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\northwest.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\previous.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\reset.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\selection-directories.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\south.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\southeast.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\southwest.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\splash-large.psd", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\splash-small.psd", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\splash.png", "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\west.png", "R:\code\contour-alignment-tool\Supporting Apps\AboutSoftware.mlapp", "R:\code\contour-alignment-tool\Supporting Apps\CheckForUpdates.mlapp", "R:\code\contour-alignment-tool\Supporting Apps\DRRviewer.mlapp", "R:\code\contour-alignment-tool\Supporting Apps\Manual.mlapp"];
buildOpts.AutoDetectDataFiles = true;
buildOpts.OutputDir = "R:\code\contour-alignment-tool\ContourAlignmentTool\for_testing";
buildOpts.Verbose = true;
buildOpts.EmbedArchive = true;
buildOpts.ExecutableIcon = "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\icon_48.png";
buildOpts.ExecutableName = "ContourAlignmentTool";
buildOpts.ExecutableSplashScreen = "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\splash.png";
buildOpts.ExecutableVersion = "1.3.7";
buildOpts.TreatInputsAsNumeric = false;
buildResult = compiler.build.standaloneWindowsApplication(buildOpts);


% Create package options object, set package properties and package.
packageOpts = compiler.package.InstallerOptions(buildResult);
packageOpts.AdditionalFiles = "R:\code\contour-alignment-tool\Dependencies";
packageOpts.AddRemoveProgramsIcon = "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\icon_48.png";
packageOpts.ApplicationName = "ContourAlignmentTool";
packageOpts.DefaultInstallationDir = "%ProgramFiles%\ContourAlignmentTool\";
packageOpts.InstallerIcon = "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\icon_48.png";
packageOpts.InstallerName = "ContourAlignmentToolInstaller";
packageOpts.InstallerSplash = "R:\code\contour-alignment-tool\ContourAlignmentTool_resources\splash.png";
packageOpts.OutputDir = "R:\code\contour-alignment-tool\ContourAlignmentTool\for_redistribution";
packageOpts.Summary = "Contour visualisation and alignment tool";
packageOpts.Verbose = true;
packageOpts.Version = "1.3.7";
compiler.package.installer(buildResult, "Options", packageOpts);

% Write flag to indicate to matlab which version this is.
fid = fopen("mode.txt","w");
fprintf(fid, "alignment");
fclose(fid);
buildOpts.AdditionalFiles{end+1} = 'mode.txt';
