% Write flag to indicate to matlab which mode this is.
mode = "alignment";
fid = fopen("mode.txt","w");
fprintf(fid, mode);
fclose(fid);

% Load version.
version = strtrim(fileread('version.txt'));

% Create target build options object, set build properties and build.
buildOpts = compiler.build.StandaloneApplicationOptions("ContourAlignmentTool.mlapp");
buildOpts.AdditionalFiles = ["ContourAlignmentTool_resources\\ROI.png", "ContourAlignmentTool_resources\\The-X-small.png", "ContourAlignmentTool_resources\\The-X.png", "ContourAlignmentTool_resources\\about.png", "ContourAlignmentTool_resources\\directories.png", "ContourAlignmentTool_resources\\auto.png", "ContourAlignmentTool_resources\\contour.png", "ContourAlignmentTool_resources\\east.png", "ContourAlignmentTool_resources\\first.png", "ContourAlignmentTool_resources\\icon.ico", "ContourAlignmentTool_resources\\icon_16.png", "ContourAlignmentTool_resources\\icon_24.png", "ContourAlignmentTool_resources\\icon_32.png", "ContourAlignmentTool_resources\\icon_48.png", "ContourAlignmentTool_resources\\instruction-PatientPlanLoad.png", "ContourAlignmentTool_resources\\instructions-alignment.png", "ContourAlignmentTool_resources\\instructions-display.png", "ContourAlignmentTool_resources\\instructions-issue.png", "ContourAlignmentTool_resources\\instructions-loading.png", "ContourAlignmentTool_resources\\instructions-saving.png", "ContourAlignmentTool_resources\\last.png", "ContourAlignmentTool_resources\\manual.png", "ContourAlignmentTool_resources\\mhaHeaderTemplate.mat", "ContourAlignmentTool_resources\\next.png", "ContourAlignmentTool_resources\\north.png", "ContourAlignmentTool_resources\\northeast.png", "ContourAlignmentTool_resources\\northwest.png", "ContourAlignmentTool_resources\\previous.png", "ContourAlignmentTool_resources\\reset.png", "ContourAlignmentTool_resources\\directories.png", "ContourAlignmentTool_resources\\south.png", "ContourAlignmentTool_resources\\southeast.png", "ContourAlignmentTool_resources\\southwest.png", "ContourAlignmentTool_resources\\splash.png", "ContourAlignmentTool_resources\\west.png", "Dependencies\\forwardprojections.exe", "Dependencies\\forwardprojectionsCUDA.exe", "Dependencies\\geometry.exe", "Dependencies\\geometryCUDA.exe", "Dependencies\\VarianReader.jar", "mode.txt", "Supporting Apps\\AboutSoftware.mlapp", "Supporting Apps\\CheckForUpdates.mlapp", "Supporting Apps\\DRRviewer.mlapp", "Supporting Apps\\Manual.mlapp", "version.txt"];
buildOpts.AutoDetectDataFiles = true;
buildOpts.OutputDir = "Deploy\\ContourAlignmentTool\\for_testing";
buildOpts.Verbose = true;
buildOpts.EmbedArchive = true;
buildOpts.ExecutableIcon = "ContourAlignmentTool_resources\\icon_48.png";
buildOpts.ExecutableName = "ContourAlignmentTool";
buildOpts.ExecutableSplashScreen = "ContourAlignmentTool_resources\\splash.png";
buildOpts.ExecutableVersion = version;
buildOpts.TreatInputsAsNumeric = false;
buildResult = compiler.build.standaloneWindowsApplication(buildOpts);


% Create package options object, set package properties and package.
packageOpts = compiler.package.InstallerOptions(buildResult);
packageOpts.AddRemoveProgramsIcon = "ContourAlignmentTool_resources\\icon_48.png";
packageOpts.ApplicationName = "ContourAlignmentTool";
packageOpts.DefaultInstallationDir = "%ProgramFiles%\\ContourAlignmentTool\\";
packageOpts.InstallerIcon = "ContourAlignmentTool_resources\\icon_48.png";
packageOpts.InstallerName = "ContourAlignmentToolInstaller";
packageOpts.InstallerSplash = "ContourAlignmentTool_resources\\splash.png";
packageOpts.OutputDir = "Deploy\\ContourAlignmentTool\\for_redistribution";
packageOpts.Summary = "Contour visualisation and alignment tool";
packageOpts.Verbose = true;
packageOpts.Version = version;
compiler.package.installer(buildResult, "Options", packageOpts);
