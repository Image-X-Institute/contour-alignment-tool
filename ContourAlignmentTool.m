classdef ContourAlignmentTool < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        ContourAlignmentToolUIFigure  matlab.ui.Figure
        FileMenu                      matlab.ui.container.Menu
        ExportMenu                    matlab.ui.container.Menu
        ExportAsMenu                  matlab.ui.container.Menu
        NewPatientMenu                matlab.ui.container.Menu
        NewFractionMenu               matlab.ui.container.Menu
        ExitMenu                      matlab.ui.container.Menu
        DisplayMenu                   matlab.ui.container.Menu
        DRRViewerMenu                 matlab.ui.container.Menu
        ContourFillMenu               matlab.ui.container.Menu
        ContourColourMenu             matlab.ui.container.Menu
        RedMenu                       matlab.ui.container.Menu
        OrangeMenu                    matlab.ui.container.Menu
        YellowMenu                    matlab.ui.container.Menu
        GreenMenu                     matlab.ui.container.Menu
        BlueMenu                      matlab.ui.container.Menu
        PurpleMenu                    matlab.ui.container.Menu
        InvertIntensityMenu           matlab.ui.container.Menu
        QuickExportMenu               matlab.ui.container.Menu
        HelpMenu                      matlab.ui.container.Menu
        ManualMenu                    matlab.ui.container.Menu
        KeyboardShortcutsMenu         matlab.ui.container.Menu
        CheckforUpdatesMenu           matlab.ui.container.Menu
        ReportMenu                    matlab.ui.container.Menu
        AboutMenu                     matlab.ui.container.Menu
        GridLayout                    matlab.ui.container.GridLayout
        DataProcessingPanel           matlab.ui.container.Panel
        ImagingTypeDropDown           matlab.ui.control.DropDown
        ImagingTypeDropDownLabel      matlab.ui.control.Label
        MasterBrowse                  matlab.ui.control.Button
        PatientDropDown               matlab.ui.control.DropDown
        FractionDropDown              matlab.ui.control.DropDown
        CTBrowse                      matlab.ui.control.Button
        CTLabel                       matlab.ui.control.Label
        StructureBrowse               matlab.ui.control.Button
        StructureLabel                matlab.ui.control.Label
        SelectStructure               matlab.ui.control.DropDown
        SelectStructureStatus         matlab.ui.control.Label
        PlanBrowse                    matlab.ui.control.Button
        PlanLabel                     matlab.ui.control.Label
        ProjectionsBrowse             matlab.ui.control.Button
        ProjectionsLabel              matlab.ui.control.Label
        ExportBrowse                  matlab.ui.control.Button
        ExportLabel                   matlab.ui.control.Label
        kVImagingParametersPanel      matlab.ui.container.Panel
        CollimatorCassetteDropDownLabel  matlab.ui.control.Label
        MachineDropDown               matlab.ui.control.DropDown
        MachineDropDownLamp           matlab.ui.control.Lamp
        MachineDropDownLabel          matlab.ui.control.Label
        MatrixSizeDropDown            matlab.ui.control.DropDown
        MatrixSizeLamp                matlab.ui.control.Lamp
        MatrixSizeLabel               matlab.ui.control.Label
        CollimatorCassetteDropDown    matlab.ui.control.DropDown
        Label                         matlab.ui.control.Label
        StartAngleLamp                matlab.ui.control.Lamp
        StartAngleValue               matlab.ui.control.NumericEditField
        StartAngleLabel               matlab.ui.control.Label
        StopAngleLamp                 matlab.ui.control.Lamp
        StopAngleValue                matlab.ui.control.NumericEditField
        StopAngleLabel                matlab.ui.control.Label
        NumberProj                    matlab.ui.control.Spinner
        AllCheckBox                   matlab.ui.control.CheckBox
        PixelSpacingLamp              matlab.ui.control.Lamp
        PixelSpacingValue             matlab.ui.control.NumericEditField
        SIDLamp                       matlab.ui.control.Lamp
        SIDvalue                      matlab.ui.control.NumericEditField
        SDDLamp                       matlab.ui.control.Lamp
        SDDvalue                      matlab.ui.control.NumericEditField
        offsetLamp                    matlab.ui.control.Lamp
        PixelspacingmmLabel           matlab.ui.control.Label
        NumberofimagestoloadLabel     matlab.ui.control.Label
        offsetValue                   matlab.ui.control.NumericEditField
        DetectoroffsetxcoordinatemmLabel  matlab.ui.control.Label
        SourcetodetectordistanceSDDmmLabel  matlab.ui.control.Label
        SourcetoisocenterdistanceSIDmmEditFieldLabel  matlab.ui.control.Label
        parametersWarning             matlab.ui.control.Label
        StartProcessing               matlab.ui.control.Button
        DirectoriesImage              matlab.ui.control.Image
        ContourAlignmentPanel         matlab.ui.container.Panel
        ClickdragCheckBox             matlab.ui.control.CheckBox
        clickdragstatus               matlab.ui.control.Label
        position                      matlab.ui.control.Label
        reset                         matlab.ui.control.Image
        ConfidenceButtonGroup         matlab.ui.container.ButtonGroup
        C0Button                      matlab.ui.control.RadioButton
        C1Button                      matlab.ui.control.RadioButton
        C2Button                      matlab.ui.control.RadioButton
        C3Button                      matlab.ui.control.RadioButton
        C4Button                      matlab.ui.control.RadioButton
        C5Button                      matlab.ui.control.RadioButton
        SW                            matlab.ui.control.Image
        NW                            matlab.ui.control.Image
        W                             matlab.ui.control.Image
        S                             matlab.ui.control.Image
        SE                            matlab.ui.control.Image
        NE                            matlab.ui.control.Image
        N                             matlab.ui.control.Image
        E                             matlab.ui.control.Image
        ContrastAdjustmentPanel       matlab.ui.container.Panel
        contrastROIButton             matlab.ui.control.StateButton
        contrastContourButton         matlab.ui.control.StateButton
        contrastAutoButton            matlab.ui.control.StateButton
        contrastManualButton          matlab.ui.control.StateButton
        UpperSlider                   matlab.ui.control.Slider
        LowerSlider                   matlab.ui.control.Slider
        Histogram                     matlab.ui.control.UIAxes
        NavigationPanel               matlab.ui.container.Panel
        First                         matlab.ui.control.Button
        Previous                      matlab.ui.control.Button
        projectionnumber              matlab.ui.control.Label
        Next                          matlab.ui.control.Button
        End                           matlab.ui.control.Button
        Tree                          matlab.ui.container.Tree
        C0Node                        matlab.ui.container.TreeNode
        C1Node                        matlab.ui.container.TreeNode
        C2Node                        matlab.ui.container.TreeNode
        C3Node                        matlab.ui.container.TreeNode
        C4Node                        matlab.ui.container.TreeNode
        C5Node                        matlab.ui.container.TreeNode
        PassedNode                    matlab.ui.container.TreeNode
        FailedNode                    matlab.ui.container.TreeNode
        ROIselectionLabel             matlab.ui.control.Label
        UIAxes                        matlab.ui.control.UIAxes
    end

    % Private properties
    properties (Access = private)

        % This is now set in file "version.txt" - added by the deploy script.
        version;

        margin = 10;  % mm. Contours within this margin are considered cropped.
        minAngle = 180;  % This is the minimum angle coverage required for a positive case in the patient selection tool.
        isRetro = false;  % Tracks if we're looking at prospective/retrospective patients in the PST.

        % Configure folder names - might need to change depending on your
        % folder structure.
        folders = struct( ...
            'default_projections_options', ["kV", "CBCT1", fullfile("CBCT", "CBCT1"), "."], ...
            'default_ct_options', ["CT", "Planning CT"], ...
            'default_rtplan_options', ["Plan"], ...
            'default_rtstruct_options', ["Structures", "Structure Set", "Structure set"], ...
            'images_options', ["PatientImages", "Patient Images", "TreatmentFiles", "Treatment Files", "Treatment files"], ...
            'plans_options', ["PatientPlans", "Patient Plans", "PlanningFiles", "Planning Files"] ...
        );

        % Parameters       
        paths
        dcmHeaders
        displaySize
        pointerManager
        
        % Images
        projection
        Projections
        Masks
        DRRs
        ROIrange

        % Supporting Apps
        AboutApp
        HelpApp
        UpdatesApp
        DrrApp
        
    end
    
    
    % Public properties
    properties (Access = public)
        % This is now set in file "mode.txt" added by the deploy script.
        mode; % This switches between "Contour Alignment Tool" and "Patient Selection Tool".
        marginX; % The margin in pixels.
        marginY;

        % Images
        originalMasks
        DRR
        Mask
        MaskOutline
        dispContour = 1;
        
        % Parameters
        fileType
        currentFrame
        colour   
        
    end

    
    
    methods (Access = private)

        
        % AUTHOR : Andy Shieh, School of Physics, The University of Sydney
        % DATE   : 2012-11-08  Created.
        % ------------------------------------------
        % PURPOSE
        %   Read metaimage mha header and image.
        %   If the file is a archived file in the format of .7z, .zip, or .rar, the
        %   program will unzip and read.
        % ------------------------------------------
        % INPUT
        %   filename : The full path to the file.
        % ------------------------------------------
        % OUTPUT
        %   info  : header information in a struct.
        %   M     : the image stored in a 3D matrix.
        % ------------------------------------------
        function [info, M] = MhaReader(app, filename)
            
            %% Checking input arguments & Opening the file
            M = [];
            
            % If no input filename => Open file-open-dialog
            if nargin < 1
                
                % go into a default directory
                DefaultDir = pwd;
                
                % get the input file (hnc) & extract path & base name
                [FileName,PathName] = uigetfile( ...
                    {'*.mha;*.7z;*.zip;*.rar','MetaImage (*.mha) or Archive file (*.7z, *.zip, *.rar)';...
                    '*.*','All files (*.*)'},...
                    'Select an image or image archive file', ...
                    DefaultDir);  
                
                % make same format as input
                filename = fullfile(PathName, FileName);
                
            end
            filename = strtrim(filename);
            
            %% Reading the header and body using external module
            info = MhaHeaderReader(app,filename);
            
            if nargout == 1
                if exist('tempdir','var')
                    rmdir(tempdir,'s');
                end
                return;
            else
                M = MhaVolumeReader(app,info);
                if exist('tempdir','var')
                    rmdir(tempdir,'s');
                end
            end
            
            return  

        end
        

        
        % AUTHOR : Andy Shieh, School of Physics, The University of Sydney
        % DATE   : 2013-01-09  Created.
        % ------------------------------------------
        % PURPOSE
        %   Write the input header and 3D image to a Metaimage file. Support only
        %   3D data.
        % ------------------------------------------
        % INPUT
        %   info:       The header MATLAB struct. The template of the info struct:
        %                                  Filename: 'SideADisBin01.mha'
        %                                    Format: 'MHA'
        %                            CompressedData: 'false'
        %                                ObjectType: 'image'
        %                        NumberOfDimensions: 3
        %                                BinaryData: 'true'
        %                                 ByteOrder: 'false'
        %                           TransformMatrix: [1 0 0 0 1 0 0 0 1]
        %                                    Offset: [-224.8400 -99 -224.8400]
        %                          CenterOfRotation: [0 0 0]
        %                     AnatomicalOrientation: 'RAI'
        %                           PixelDimensions: [0.8800 2 0.8800]
        %                                Dimensions: [512 100 512]
        %                                  DataType: 'float'
        %                                  DataFile: 'LOCAL'
        %                                  BitDepth: 32
        %                                HeaderSize: 318
        %   M:          The image body, 3D.
        %   outputfp:   The path of the output file.
        % ------------------------------------------
        function MhaWriter(~,info,M,outputfp)
            
            %% Input argument check            
            if ~(strcmp(info.DataType,'float') || strcmp(info.DataType,'double') || ...
                    strcmp(info.DataType,'int8') || strcmp(info.DataType,'char') || ...
                    strcmp(info.DataType,'uint8') || strcmp(info.DataType,'uchar') || ...
                    strcmp(info.DataType,'int16') || strcmp(info.DataType,'short') || ...
                    strcmp(info.DataType,'uint16') || strcmp(info.DataType,'ushort') || ...
                    strcmp(info.DataType,'int'))
                error('ERROR: Unsupported data type.');
            end
            
            if ~strcmp(info.CompressedData,'false')
                error('ERROR: MHA is an uncompressed format.');
            end
            
            if ~strcmp(info.DataFile,'LOCAL')
                error('ERROR: MHA is a single file format.');
            end
            
            fid = fopen(strtrim(outputfp),'w');
            if(fid<=0) 
                error('ERROR: Invalid file path %s\n', outputfp);
            end
            
            %% Writing the header
            fprintf(fid, 'ObjectType = Image\n');
            fprintf(fid, num2str(info.NumberOfDimensions,'NDims = %d\\n'));
            if isfield(info,'BinaryData')
                fprintf(fid, ['BinaryData = ',info.BinaryData,'\n']);
            end
            fprintf(fid, ['BinaryDataByteOrderMSB = ',info.ByteOrder,'\n']);
            fprintf(fid, ['CompressedData = ',info.CompressedData,'\n']);
            if isfield(info,'TransformMatrix')
                fprintf(fid, ['TransformMatrix =',num2str(info.TransformMatrix,' %g'),'\n']);
            end
            fprintf(fid, ['Offset =',num2str(info.Offset,' %g'),'\n']);
            if isfield(info,'CenterOfRotation')
                fprintf(fid, ['CenterOfRotation =',num2str(info.CenterOfRotation,' %g'),'\n']);
            end
            if isfield(info,'AnatomicalOrientation')
                fprintf(fid, ['AnatomicalOrientation = ',info.AnatomicalOrientation,'\n']);
            end
            fprintf(fid, ['ElementSpacing =',num2str(info.PixelDimensions,' %g'),'\n']);
            fprintf(fid, ['DimSize =',num2str(info.Dimensions,' %d'),'\n']);
            if isfield(info,'ElementNumberOfChannels')
                fprintf(fid, ['ElementNumberOfChannels =',num2str(info.ElementNumberOfChannels,' %d'),'\n']);
            end
            switch (info.DataType)
                case 'float'
                    fprintf(fid, 'ElementType = MET_FLOAT\n');
                case 'double'
                    fprintf(fid, 'ElementType = MET_DOUBLE\n');
                case {'uchar','uint8'}
                    fprintf(fid, 'ElementType = MET_UCHAR\n');
                case {'char','int8'}
                    fprintf(fid, 'ElementType = MET_CHAR\n');   
                case {'short','int16'}
                    fprintf(fid, 'ElementType = MET_SHORT\n');
                case {'ushort','uint16'}
                    fprintf(fid, 'ElementType = MET_USHORT\n');
                case 'int'
                    fprintf(fid, 'ElementType = MET_INT\n');
            end
            fprintf(fid, ['ElementDataFile = ',info.DataFile,'\n']);
            
            %% Writing the body
            
            % Need to reverse the dimension order if there are multi-entries
            if isfield(info,'ElementNumberOfChannels') && info.ElementNumberOfChannels > 1
                M = reshape(M,[prod(info.Dimensions),str2double(info.ElementNumberOfChannels)]);
                M = M';
            end
            
            switch(info.DataType)
                case {'char','int8'}
                    fwrite(fid,M,'char');
                case {'uchar','uint8'}
                    fwrite(fid,M,'uchar');
                case {'short','int16'}
                    fwrite(fid,M,'short');
                case {'ushort','uint16'}
                    fwrite(fid,M,'ushort');
                case 'int'
                    fwrite(fid,M,'int');
                case 'uint'
                    fwrite(fid,M,'uint');
                case 'float'
                    fwrite(fid,M,'float');
                case 'double'
                    fwrite(fid,M,'double');
            end
            
            fclose(fid);
            
            return
        end
        
        
        
        % From open source: https://github.com/FNNDSC/matlab/blob/master/misc/mha_read_header.m
        % Function for reading the header of a Insight Meta-Image (.mha,.mhd) file
        function info = MhaHeaderReader(~,filename)
            info = struct();

            if(exist('filename','var')==0)
                [filename, pathname] = uigetfile('*.mha', 'Read mha-file');
                filename = [pathname filename];
            end

            fid=fopen(filename,'rb');
            if(fid<0)
                error('MhaHeaderReader:FileOpenFailed', 'Could not open file %s', filename);
            end
            
            info.Filename=filename;
            info.Format='MHA';
            info.CompressedData='false';
            readelementdatafile=false;
            while(~readelementdatafile)
                str=fgetl(fid);
                s=find(str=='=',1,'first');
                if(~isempty(s))
                    type=str(1:s-1); 
                    data=str(s+1:end);
                    while(type(end)==' '); type=type(1:end-1); end
                    while(data(1)==' '); data=data(2:end); end
                else
                    type=''; data=str;
                end
                
                switch(lower(type))
                    case 'ndims'
                        info.NumberOfDimensions=sscanf(data, '%d')';
                    case 'dimsize'
                        info.Dimensions=sscanf(data, '%d')';
                    case 'elementspacing'
                        info.PixelDimensions=sscanf(data, '%lf')';
                    case 'elementsize'
                        info.ElementSize=sscanf(data, '%lf')';
                        if(~isfield(info,'PixelDimensions'))
                            info.PixelDimensions=info.ElementSize;
                        end
                    case 'elementbyteordermsb'
                        info.ByteOrder=lower(data);
                    case 'anatomicalorientation'
                        info.AnatomicalOrientation=data;
                    case 'centerofrotation'
                        info.CenterOfRotation=sscanf(data, '%lf')';
                    case 'offset'
                        info.Offset=sscanf(data, '%lf')';
                    case 'binarydata'
                        info.BinaryData=lower(data);
                    case 'compresseddatasize'
                        info.CompressedDataSize=sscanf(data, '%d')';
                    case 'objecttype'
                        info.ObjectType=lower(data);
                    case 'transformmatrix'
                        info.TransformMatrix=sscanf(data, '%lf')';
                    case 'compresseddata'
                        info.CompressedData=lower(data);
                    case 'binarydatabyteordermsb'
                        info.ByteOrder=lower(data);
                    case 'elementdatafile'
                        info.DataFile=data;
                        readelementdatafile=true;
                    case 'elementtype'
                        info.DataType=lower(data(5:end));
                    case 'headersize'
                        val=sscanf(data, '%d')';
                        if(val(1)>0), info.HeaderSize=val(1); end
                    otherwise
                        info.(type)=data;
                end
            end
            
            switch(info.DataType)
                case 'char', info.BitDepth=8;
                case 'uchar', info.BitDepth=8;
                case 'short', info.BitDepth=16;
                case 'ushort', info.BitDepth=16;
                case 'int', info.BitDepth=32;
                case 'uint', info.BitDepth=32;
                case 'float', info.BitDepth=32;
                case 'double', info.BitDepth=64;
                otherwise, info.BitDepth=0;
            end
            if(~isfield(info,'HeaderSize'))
                info.HeaderSize=ftell(fid);
            end
            fclose(fid);

        end
        
        
        
        % Modified from open source: https://github.com/FNNDSC/matlab/blob/master/misc/mha_read_volume.m
        % Function for reading the volume of a Insight Meta-Image (.mha, .mhd) file
        function [V,info] = MhaVolumeReader(app,info)
            
            if(~isstruct(info)), info=MhaHeaderReader(app,info); end
            
            
            switch(lower(info.DataFile))
                case 'local'
                otherwise
                % Seperate file
                info.Filename=fullfile(fileparts(info.Filename),info.DataFile);
            end
                    
            % Open file
            switch(info.ByteOrder(1))
                case ('true')
                    fid=fopen(info.Filename,'rb','ieee-be');
                otherwise
                    fid=fopen(info.Filename,'rb','ieee-le');
            end
            
            datasize=prod(info.Dimensions)*info.BitDepth/8;
            datasummary = dir(info.Filename);
            totalsize = datasummary.bytes;
            
            switch(lower(info.DataFile))
                case 'local'
                    fseek(fid,mod(totalsize,datasize),'bof');
                otherwise
                    fseek(fid,0,'bof');
            end
            
            switch(info.CompressedData(1))
                case 'f'
                    % Read the Data
                    switch(info.DataType)
                        case 'char'
                            V = int8(fread(fid,datasize,'char')); 
                        case 'uchar'
                            V = uint8(fread(fid,datasize,'uchar')); 
                        case 'short'
                            V = int16(fread(fid,datasize,'short')); 
                        case 'ushort'
                            V = uint16(fread(fid,datasize,'ushort')); 
                        case 'int'
                            V = int32(fread(fid,datasize,'int')); 
                        case 'uint'
                            V = uint32(fread(fid,datasize,'uint')); 
                        case 'float'
                            V = single(fread(fid,datasize,'float'));   
                        case 'double'
                            V = double(fread(fid,datasize,'double'));
                    end
                case 't'
                    error('Data is compressed. \n');
            
            end
            fclose(fid);
            
            % Support reading multi-entries (e.g. vector field)
            if isfield(info,'ElementNumberOfChannels') && info.ElementNumberOfChannels > 1
                V = reshape(V,[str2double(info.ElementNumberOfChannels),prod(info.Dimensions)]);
                V = V';
                V = reshape(V,[info.Dimensions,str2double(info.ElementNumberOfChannels)]);
            else
                V = reshape(V,info.Dimensions);
            end
        end        
        
        % Convert the RTStructure to a MHA volume
        function [Segmentation] = RTStructuretoMHA(app, structure, zLocs_sorted)
            
            try
                RSinfo = dicominfo(app.paths.structure); 
                contour = dicomContours(RSinfo);
            catch
                RSinfo = dicominfo(app.paths.structure,'UseVRHeuristic',false);
                contour = dicomContours(RSinfo);
            end
            
            index = find(strcmp(contour.ROIs{:,2},structure));
            
            % Build affine transformation
            dr = app.dcmHeaders{1}.PixelSpacing(1);
            dc = app.dcmHeaders{1}.PixelSpacing(2);
            F(:,1) = app.dcmHeaders{1}.ImageOrientationPatient(1:3);
            F(:,2) = app.dcmHeaders{1}.ImageOrientationPatient(4:6);
            
            T1 = [app.dcmHeaders{1}.ImagePositionPatient(1:2); zLocs_sorted{1}(1)];
            TN = [app.dcmHeaders{1}.ImagePositionPatient(1:2); zLocs_sorted{1}(end)];
            
            offset = (T1 - TN) ./ (1 - length(app.dcmHeaders));
              
            % Build affine transformation
            xfm = [[F(1,1)*dr F(1,2)*dc offset(1) T1(1)]; ...
                 [F(2,1)*dr F(2,2)*dc offset(2) T1(2)]; ...
                 [F(3,1)*dr F(3,2)*dc offset(3) T1(3)]; ...
                 [0         0         0    1    ]];

            %% From readRTStructures. 
            dimmin = [0 0 0 1]';
            dimmax = double([app.dcmHeaders{1}.Columns-1 app.dcmHeaders{1}.Rows-1 length(app.dcmHeaders)-1 1])';
            
            Segmentation = false([app.dcmHeaders{1}.Columns app.dcmHeaders{1}.Rows length(app.dcmHeaders)]);
            
            ROIContourSequence = fieldnames(RSinfo.ROIContourSequence);
            ContourSequence = fieldnames(RSinfo.ROIContourSequence.(ROIContourSequence{index}).ContourSequence);
            
            %% Loop through segments (slices)
            segments = cell(1,length(ContourSequence));
            for j = 1:length(ContourSequence)
              
                %% Read points
                segments{j} = reshape(RSinfo.ROIContourSequence.(ROIContourSequence{index}).ContourSequence.(ContourSequence{j}).ContourData, ...
                  3, RSinfo.ROIContourSequence.(ROIContourSequence{index}).ContourSequence.(ContourSequence{j}).NumberOfContourPoints)';
                
                %% Make lattice
                points = xfm \ [segments{j} ones(size(segments{j},1), 1)]';
                start = xfm \ [segments{j}(1,:) 1]';
                minvox = max(floor(min(points, [], 2)), dimmin);
                maxvox = min( ceil(max(points, [], 2)), dimmax);
                minvox(3) = round(start(3));
                maxvox(3) = round(start(3));
                [x,y,z] = meshgrid(minvox(1):maxvox(1), minvox(2):maxvox(2), minvox(3):maxvox(3));
                points = xfm * [x(:) y(:) z(:) ones(size(x(:)))]';
                
                %% Make binary image
                in = inpolygon(points(1,:), points(2,:), segments{j}(:,1), segments{j}(:,2));
                Segmentation((minvox(1):maxvox(1))+1, (minvox(2):maxvox(2))+1, (minvox(3):maxvox(3))+1) = permute(reshape(in, size(x)), [2 1]); 
            end
        end
        
        % AUTHOR : Andy Shieh, School of Physics, The University of Sydney
        % DATE   : 2016-01-10  Created.
        % ------------------------------------------
        % PURPOSE
        %   Read Varian hnd header and image.
        % ------------------------------------------
        % INPUT
        %   filename : The full path to the hnd file.
        % ------------------------------------------
        % OUTPUT
        %   info  : Header information in a struct.
        %   M     : The image stored in a 2D matrix.
        % ------------------------------------------
        function [info, M] = HndReader(~, filename)
            
            %% Checking input arguments & Opening the file
            
            M = [];
            
            % If no input filename => Open file-open-dialog
            if nargin < 1
                
                % go into a default directory
                DefaultDir = pwd;
                
                % get the input file (hnc) & extract path & base name
                [FileName,PathName] = uigetfile( {'*.hnd;*.hnc;','Varian Image Files (*.hnd,*.hnc,*.mat,*.mdl)';
                    '*.hnc',  'Portal Images (*.hnc)'; ...
                    '*.hnd',  'OBI kv Images (*.hnd)'}, ...
                    'Select an image file', ...
                    DefaultDir);
                
                % make same format as input
                filename = fullfile(PathName, FileName); 
            end
            
            filename = strtrim(filename);
            
            % Open the file
            fid = fopen(filename,'r');
            
                % catch error if failure to open the file
                if fid == -1
                    error('ERROR : Failure in opening the file. \n');
                end
            
            %% Reading the header (same for hnc and hnd)
            
            info.('bFileType') = fread(fid,32,'uint8=>char')';
            info.('uiFileLength') = fread(fid,1,'uint32')';
            
            % Sometimes the actual file length is different from the one specified in
            % the header
            fileinfo = dir(filename);
            info.('uiActualFileLength') = fileinfo.('bytes');
            
            info.('bChecksumSpec') = fread(fid,4,'uint8=>char')';
            info.('uiCheckSum') = fread(fid,1,'uint32')';
            info.('bCreationDate') = fread(fid,8,'uint8=>char')';
            info.('bCreationTime') = fread(fid,8,'uint8=>char')';
            info.('bPatientID') = fread(fid,16,'uint8=>char')';
            info.('uiPatientSer') = fread(fid,1,'uint32')';
            info.('bSeriesID') = fread(fid,16,'uint8=>char')';
            info.('uiSeriesSer') = fread(fid,1,'uint32')';
            info.('bSliceID') = fread(fid,16,'uint8=>char')';
            info.('uiSliceSer') = fread(fid,1,'uint32')';
            info.('uiSizeX') = fread(fid,1,'uint32')';
            info.('uiSizeY') = fread(fid,1,'uint32')';
            info.('dSliceZPos') = fread(fid,1,'double')';
            info.('bModality') = fread(fid,16,'uint8=>char')';
            info.('uiWindow') = fread(fid,1,'uint32')';
            info.('uiLevel') = fread(fid,1,'uint32')';
            info.('uiPixelOffset') = fread(fid,1,'uint32')';
            info.('bImageType') = fread(fid,4,'uint8=>char')';
            info.('dGantryRtn') = fread(fid,1,'double')';
            info.('dSAD') = fread(fid,1,'double')';
            info.('dSFD') = fread(fid,1,'double')';
            info.('dCollX1') = fread(fid,1,'double')';
            info.('dCollX2') = fread(fid,1,'double')';
            info.('dCollY1') = fread(fid,1,'double')';
            info.('dCollY2') = fread(fid,1,'double')';
            info.('dCollRtn') = fread(fid,1,'double')';
            info.('dFieldX') = fread(fid,1,'double')';
            info.('dFieldY') = fread(fid,1,'double')';
            info.('dBladeX1') = fread(fid,1,'double')';
            info.('dBladeX2') = fread(fid,1,'double')';
            info.('dBladeY1') = fread(fid,1,'double')';
            info.('dBladeY2') = fread(fid,1,'double')';
            info.('dIDUPosLng') = fread(fid,1,'double')';
            info.('dIDUPosLat') = fread(fid,1,'double')';
            info.('dIDUPosVrt') = fread(fid,1,'double')';
            info.('dIDUPosRtn') = fread(fid,1,'double')';
            info.('dPatientSupportAngle') = fread(fid,1,'double')';
            info.('dTableTopEccentricAngle') = fread(fid,1,'double')';
            info.('dCouchVrt') = fread(fid,1,'double')';
            info.('dCouchLng') = fread(fid,1,'double')';
            info.('dCouchLat') = fread(fid,1,'double')';
            info.('dIDUResolutionX') = fread(fid,1,'double')';
            info.('dIDUResolutionY') = fread(fid,1,'double')';
            info.('dImageResolutionX') = fread(fid,1,'double')';
            info.('dImageResolutionY') = fread(fid,1,'double')';
            info.('dEnergy') = fread(fid,1,'double')';
            info.('dDoseRate') = fread(fid,1,'double')';
            info.('dXRayKV') = fread(fid,1,'double')';
            info.('dXRayMA') = fread(fid,1,'double')';
            info.('dMetersetExposure') = fread(fid,1,'double')';
            info.('dAcqAdjustment') = fread(fid,1,'double')';
            info.('dCTProjectionAngle') = fread(fid,1,'double')';
            info.('dCBCTPositiveAngle') = mod(info.('dCTProjectionAngle') + 270.0, 360.0);
            info.('dCTNormChamber') = fread(fid,1,'double')';
            info.('dGatingTimeTag') = fread(fid,1,'double')';
            info.('dGating4DInfoX') = fread(fid,1,'double')';
            info.('dGating4DInfoY') = fread(fid,1,'double')';
            info.('dGating4DInfoZ') = fread(fid,1,'double')';
            info.('dGating4DInfoTime') = fread(fid,1,'double')';
            info.('dOffsetX') = fread(fid,1,'double');
            info.('dOffsetY') = fread(fid,1,'double');
            info.('dUnusedField') = fread(fid,1,'double');
            
            if nargout == 1
                fclose(fid);
                return
            end
            
            fclose(fid);
            
            %% Use Varian Image toolbox to read the image body
            javaaddpath(fullfile(pwd,'Dependencies', 'VarianReader.jar'));
            
            readerObj = CPS_Reader();
            M = readerObj.getHNC_HND(filename);
            clear readerObj;
            javarmpath(fullfile(pwd,'Dependencies', 'VarianReader.jar'));
            
            % Reshape the projection data matrix
            M = reshape(M,info.('uiSizeX'),info.('uiSizeY'));
            
            return

        end
        
        
        
        % AUTHOR : Andy Shieh, The University of Sydney
        % DATE   : 2016-10-11  Created.
        % ------------------------------------------
        % PURPOSE
        %   Read Varian XIM image and header.
        %   This program was adapted from the original version developed by
        %   Fredrik Nordström in 2015.
        % ------------------------------------------
        % INPUT
        %   filename : The full path to the xim file.
        % ------------------------------------------
        % OUTPUT
        %   info  : Header information in a struct.
        %   M     : The image stored in a 2D matrix.
        % ------------------------------------------
        function [info,M] = XimReader(~,filename)
            %% Input filename
            if nargin < 1
                filename = uigetfilepath( {'*.xim;*.XIM;','Varian XIM Files (*.xim,*.XIM)';
                    '*.xim',  'Varian XIM Files (*.xim)'; ...
                    '*.XIM',  'Varian XIM Files (*.XIM)'}, ...
                    'Select an image file', ...
                    pwd);
            end
            
            
            %% Read header
            fid = fopen(filename,'r');
            
            % Decode header
            info.file_name = filename;
            info.file_format_identifier = fread(fid,8,'*char')';
            info.file_format_version = fread(fid,1,'*int32');
            info.image_width = fread(fid,1,'*int32');
            info.image_height = fread(fid,1,'*int32');
            info.bits_per_pixel = fread(fid,1,'*int32');
            info.bytes_per_pixel = fread(fid,1,'*int32');
            info.compression_indicator = fread(fid,1,'*int32');
            
            %% Read image
            if nargout < 2
                read_pixel_data = 0;
            else
                read_pixel_data = 1;
            end
            
            % Decode pixel data
            if info.compression_indicator == 1
                lookup_table_size = fread(fid,1,'int32');    
                lookup_table = fread(fid,lookup_table_size*4,'ubit2=>uint8');
                compressed_pixel_buffer_size = fread(fid,1,'int32');
                if read_pixel_data==1
                    disp('reading pixel data')
                    % Decompress image
                    % BC: This uses variable-length, differential compression as neighbouring
                    % pixels are assumed to have similar intensity values.
                    pixel_data = int32(zeros(info.image_width*info.image_height,1));
                    pixel_data(1:info.image_width+1) = fread(fid,info.image_width+1,'*int32');
                    disp('read')

                    % Switch between old/new decompression versions.
                    decompress_method = "old";
                    if decompress_method == "old"
                        lookup_table_pos = 1;
                        for image_pos = (info.image_width+2):(info.image_width*info.image_height)  
                            % Variable-length: how many bytes are needed to store the diff?
                            if lookup_table(lookup_table_pos) == 0
                                diff = int32(fread(fid,1,'*int8'));
                            elseif lookup_table(lookup_table_pos) == 1
                                diff = int32(fread(fid,1,'*int16'));
                            else
                                diff = int32(fread(fid,1,'*int32'));
                            end
                            % Differential: add the left/top/top-left pixel values.
                            pixel_data(image_pos) = ...
                                diff+pixel_data(image_pos-1) + ...
                                pixel_data(image_pos-info.image_width) - ...
                                pixel_data(image_pos-info.image_width-1);
                            lookup_table_pos = lookup_table_pos + 1;
                        end
                    else

                        % BC: This didn't work for all patients -
                        % issue for Prostate/Pat02/Fx2

                        % New lookup table stuff.
                        % Although diffs are stored as variable-length, we can read them all as the lowest
                        % possible length (single byte) and reconstruct them in matlab. The purpose is to 
                        % reduce the number of filesystem ('fread') calls from N=<size of image> to N=1.
                        
                        % Read all bytes.
                        n_bytes = sum(lookup_table == 0) * 1 + ...
                              sum(lookup_table == 1) * 2 + ...
                              sum(lookup_table == 2) * 4;
                        raw_bytes = fread(fid, n_bytes, '*uint8');
    
                        % Compute actual intensities.
                        lookup_table_pos = 1;
                        raw_bytes_pos = 1;
                        for image_pos = (info.image_width+2):(info.image_width*info.image_height)  
                            % Variable-length: how many bytes are needed to store the diff?
                            if lookup_table(lookup_table_pos) == 0
                                diff = typecast(raw_bytes(raw_bytes_pos), 'int8');
                                raw_bytes_pos = raw_bytes_pos + 1;
                            elseif lookup_table(lookup_table_pos) == 1
                                diff = typecast(raw_bytes(raw_bytes_pos:raw_bytes_pos + 1), 'int16');
                                raw_bytes_pos = raw_bytes_pos + 2;
                            else
                                diff = typecast(raw_bytes(raw_bytes_pos:raw_bytes_pos + 3), 'int32');
                                raw_bytes_pos = raw_bytes_pos + 4;
                            end
    
                            % Differential: add the left/top/top-left pixel values.
                            pixel_data(image_pos) = ...
                                int32(diff) + pixel_data(image_pos-1) + ...
                                pixel_data(image_pos-info.image_width) - ...
                                pixel_data(image_pos-info.image_width-1);
    
                            lookup_table_pos = lookup_table_pos + 1;
                        end

                    end

                    disp('lookup table stuff')
                    
                    if info.bytes_per_pixel == 2
                        info.pixel_data = ...
                            int16(reshape(pixel_data,info.image_width,info.image_height));
                    else
                        info.pixel_data = ...
                            reshape(pixel_data,info.image_width,info.image_height);
                    end

                    disp('reshaping stuff')
                else
                    fseek(fid,compressed_pixel_buffer_size,'cof');
                end
                uncompressed_pixel_buffer_size = fread(fid,1,'*int32'); 
            else
                disp('loading uncompressed xim')
                uncompressed_pixel_buffer_size = fread(fid,1,'*int32');
                if read_pixel_data == 1    
                    disp('reading pixel data')
                    switch info.bytes_per_pixel
                        case 1
                            pixel_data = fread(fid,uncompressed_pixel_buffer_size,'*int8');
                        case 2
                            pixel_data = fread(fid,uncompressed_pixel_buffer_size/2,'*int16');
                        otherwise
                            pixel_data = fread(fid,uncompressed_pixel_buffer_size/4,'*int32');
                    end
                    disp('read')
                    info.pixel_data = reshape(pixel_data,info.image_width,info.image_height);
                else
                    % fseek(fid,uncompressed_pixel_buffer_size,'cof');
                    switch info.bytes_per_pixel
                        case 1
                            fseek(fid, uncompressed_pixel_buffer_size, 'cof');
                        case 2
                            fseek(fid, uncompressed_pixel_buffer_size/2, 'cof');
                        otherwise
                            fseek(fid, uncompressed_pixel_buffer_size/4, 'cof');
                    end
                end
            end
            
            % Decode histogram
            number_of_bins_in_histogram = fread(fid,1,'*int32');
            if number_of_bins_in_histogram>0
                info.histogram.number_of_bins_in_histogram=number_of_bins_in_histogram;
                info.histogram.histogram_data = fread(fid,number_of_bins_in_histogram,'*int32');
            end
            
            % Decode properties
            number_of_properties = fread(fid,1,'*int32');
            if number_of_properties>0
                info.properties=[];
            end
            for property_nr=1:number_of_properties
                property_name_length = fread(fid,1,'*int32');
                property_name = fread(fid,property_name_length,'*char')';
                property_type = fread(fid,1,'*int32');
                switch property_type
                    case 0
                        property_value = fread(fid,1,'*int32');
                    case 1
                        property_value = fread(fid,1,'double');
                    case 2
                        property_value_length = fread(fid,1,'*int32');
                        property_value = fread(fid,property_value_length,'*char')';
                    case 4
                        property_value_length = fread(fid,1,'*int32');
                        property_value = fread(fid,property_value_length/8,'double');
                    case 5
                        property_value_length = fread(fid,1,'*int32');
                        property_value = fread(fid,property_value_length/4,'*int32');
                    otherwise
                        disp(' ')
                        disp([property_name ': Property type ' num2str(property_type) ' is not supported! Aborting property decoding!']);
                        fclose(fid);
                        return;
                end
                info.properties=setfield(info.properties,property_name,property_value);
            end
            
            if nargout > 1
                M = info.pixel_data;
                rmfield(info,'pixel_data');
            end
            
            fclose(fid);
 
        end
        
        
        
        % AUTHOR : Andy Shieh, School of Physics, The University of Sydney
        % DATE   : 2014-06-02  Created.
        % ------------------------------------------
        % PURPOSE
        %   Read his header and image.
        % ------------------------------------------
        % INPUT
        %   filename : The full path to the hnc file.
        % ------------------------------------------
        % OUTPUT
        %   info  : Header information (in uint16 un-decoded)
        %   M     : The image stored in a 2D matrix (uint16).
        % ------------------------------------------
        function [info,M] = HisReader(~,filename)

            %% Checking input arguments & Opening the file
            
            M = [];
            
            HISHeadLength = 68;
            ElektaDetSizeX = 409.6;
            ElektaDetSizeY = 409.6;
            
            % If no input filename => Open file-open-dialog
            if nargin < 1
                
                % go into a default directory
                DefaultDir = pwd;
                
                % get the input file (hnc) & extract path & base name
                [FileName,PathName] = uigetfile( {'*.his;','Elekta Image Files (*.his)'}, ...
                    'Select an image file', ...
                    DefaultDir);
                
                % make same format as input
                filename = fullfile(PathName, FileName);
            end
            
            filename = strtrim(filename);
            
            % Open the file
            fid = fopen(filename,'r');
            
                % catch error if failure to open the file
                if fid == -1
                    error('ERROR : Failure in opening the file. \n');
                end
            
            %% Reading the header
            
            fileinfo = dir(filename);
            info.('uiActualFileLength') = fileinfo.('bytes');
            
            header = fread(fid,HISHeadLength,'int8');
            if (header(1) ~=0 || header(2) ~= 112 || header(3) ~= 68 || header(4) ~=0)
                error(['ERROR: File:',filename,' is not in Heimann HIS format version 100']);
            end
            
            info.('HeaderSize') = header(11) + bitshift(header(12),8) + HISHeadLength;
            ulx = header(13) + bitshift(header(14),8);
            uly = header(15) + bitshift(header(16),8);
            brx = header(17) + bitshift(header(18),8);
            bry = header(19) + bitshift(header(20),8);
            info.('NFrames') = header(21) + bitshift(header(22),8);
            info.('Type') = header(33) + bitshift(header(35),8);
            
            info.('SizeX') = bry-uly+1;
            info.('SizeY') = brx-ulx+1;
            info.('PixelSpacingX') = ElektaDetSizeX / info.SizeX;
            info.('PixelSpacingY') = ElektaDetSizeY / info.SizeY;
            info.('OriginX') = -0.5 * (info.('SizeX') - 1) * info.('PixelSpacingX');
            info.('OriginY') = -0.5 * (info.('SizeY') - 1) * info.('PixelSpacingY');
            
            if nargout == 1
                fclose(fid);
                return
            end
            
            %% Reading the projection
            
            % Calculate bytes per pixels
            nPixels = info.('SizeX') * info.('SizeY') * info.('NFrames');
            
            % Read the projection according to the bytes per pixel information
            fseek(fid,info.HeaderSize,-1);
            switch info.('Type')
                case (4)
                    M = uint16(fread(fid,nPixels,'uint16'));
                otherwise
                    M = uint16(fread(fid,nPixels,'uint16'));
            end
            
            % Reshape the projection data matrix
            M = reshape(M,info.('SizeX'),info.('SizeY'),info.('NFrames'));
            
            % Close the file
            fclose(fid);
            
            return

        end
        
        
        
        % AUTHOR : Andy Shieh, School of Physics, The University of Sydney
        % DATE   : 2012-10-26  Created.
        % ------------------------------------------
        % PURPOSE
        %   Read Varian hnc header and image. Does not account for dual gain.
        % ------------------------------------------
        % INPUT
        %   filename : The full path to the hnc file.
        % ------------------------------------------
        % OUTPUT
        %   info  : Header information in a struct.
        %   M     : The image stored in a 2D matrix (uint16).
        % ------------------------------------------
        function [info, M] = HncReader(~, filename)
            
            %% Checking input arguments & Opening the file
            
            M = [];
            
            % If no input filename => Open file-open-dialog
            if nargin < 1
                
                % go into a default directory
                DefaultDir = pwd;
                
                % get the input file (hnc) & extract path & base name
                [FileName,PathName] = uigetfile( {'*.hnd;*.hnc;','Varian Image Files (*.hnd,*.hnc,*.mat,*.mdl)';
                    '*.hnc',  'Portal Images (*.hnc)'; ...
                    '*.hnd',  'OBI kv Images (*.hnd)'}, ...
                    'Select an image file', ...
                    DefaultDir);
                
                % make same format as input
                filename = fullfile(PathName, FileName); 
            end
            
            filename = strtrim(filename);
            
            % Open the file
            fid = fopen(filename,'r');
            
                % catch error if failure to open the file
                if fid == -1
                    error('ERROR : Failure in opening the file. \n');
                end
            
            %% Reading the header (same for hnc and hnd)
            
            info.('bFileType') = fread(fid,32,'uint8=>char')';
            info.('uiFileLength') = fread(fid,1,'uint32')';
            
            % Sometimes the actual file length is different from the one specified in
            % the header
            fileinfo = dir(filename);
            info.('uiActualFileLength') = fileinfo.('bytes');
            
            info.('bChecksumSpec') = fread(fid,4,'uint8=>char')';
            info.('uiCheckSum') = fread(fid,1,'uint32')';
            info.('bCreationDate') = fread(fid,8,'uint8=>char')';
            info.('bCreationTime') = fread(fid,8,'uint8=>char')';
            info.('bPatientID') = fread(fid,16,'uint8=>char')';
            info.('uiPatientSer') = fread(fid,1,'uint32')';
            info.('bSeriesID') = fread(fid,16,'uint8=>char')';
            info.('uiSeriesSer') = fread(fid,1,'uint32')';
            info.('bSliceID') = fread(fid,16,'uint8=>char')';
            info.('uiSliceSer') = fread(fid,1,'uint32')';
            info.('uiSizeX') = fread(fid,1,'uint32')';
            info.('uiSizeY') = fread(fid,1,'uint32')';
            info.('dSliceZPos') = fread(fid,1,'double')';
            info.('bModality') = fread(fid,16,'uint8=>char')';
            info.('uiWindow') = fread(fid,1,'uint32')';
            info.('uiLevel') = fread(fid,1,'uint32')';
            info.('uiPixelOffset') = fread(fid,1,'uint32')';
            info.('bImageType') = fread(fid,4,'uint8=>char')';
            info.('dGantryRtn') = fread(fid,1,'double')';
            info.('dSAD') = fread(fid,1,'double')';
            info.('dSFD') = fread(fid,1,'double')';
            info.('dCollX1') = fread(fid,1,'double')';
            info.('dCollX2') = fread(fid,1,'double')';
            info.('dCollY1') = fread(fid,1,'double')';
            info.('dCollY2') = fread(fid,1,'double')';
            info.('dCollRtn') = fread(fid,1,'double')';
            info.('dFieldX') = fread(fid,1,'double')';
            info.('dFieldY') = fread(fid,1,'double')';
            info.('dBladeX1') = fread(fid,1,'double')';
            info.('dBladeX2') = fread(fid,1,'double')';
            info.('dBladeY1') = fread(fid,1,'double')';
            info.('dBladeY2') = fread(fid,1,'double')';
            info.('dIDUPosLng') = fread(fid,1,'double')';
            info.('dIDUPosLat') = fread(fid,1,'double')';
            info.('dIDUPosVrt') = fread(fid,1,'double')';
            info.('dIDUPosRtn') = fread(fid,1,'double')';
            info.('dPatientSupportAngle') = fread(fid,1,'double')';
            info.('dTableTopEccentricAngle') = fread(fid,1,'double')';
            info.('dCouchVrt') = fread(fid,1,'double')';
            info.('dCouchLng') = fread(fid,1,'double')';
            info.('dCouchLat') = fread(fid,1,'double')';
            info.('dIDUResolutionX') = fread(fid,1,'double')';
            info.('dIDUResolutionY') = fread(fid,1,'double')';
            info.('dImageResolutionX') = fread(fid,1,'double')';
            info.('dImageResolutionY') = fread(fid,1,'double')';
            info.('dEnergy') = fread(fid,1,'double')';
            info.('dDoseRate') = fread(fid,1,'double')';
            info.('dXRayKV') = fread(fid,1,'double')';
            info.('dXRayMA') = fread(fid,1,'double')';
            info.('dMetersetExposure') = fread(fid,1,'double')';
            info.('dAcqAdjustment') = fread(fid,1,'double')';
            info.('dCTProjectionAngle') = fread(fid,1,'double')';
            info.('dCBCTPositiveAngle') = mod(info.('dCTProjectionAngle') + 270.0, 360.0);
            info.('dCTNormChamber') = fread(fid,1,'double')';
            info.('dGatingTimeTag') = fread(fid,1,'double')';
            info.('dGating4DInfoX') = fread(fid,1,'double')';
            info.('dGating4DInfoY') = fread(fid,1,'double')';
            info.('dGating4DInfoZ') = fread(fid,1,'double')';
            info.('dGating4DInfoTime') = fread(fid,1,'double')';
            info.('dOffsetX') = fread(fid,1,'double');
            info.('dOffsetY') = fread(fid,1,'double');
            info.('dUnusedField') = fread(fid,1,'double');
            
            if nargout == 1
                fclose(fid);
                return
            end
            
            %% Reading the projection
            
            % Calculate bytes per pixels
            nPixels = info.('uiSizeX') * info.('uiSizeY');
            
                % Check if the number of elements is correct
                if mod( info.('uiActualFileLength') - ftell(fid), nPixels) ~= 0
                    fclose(fid);
                    error('ERROR: Incompatible file format. \n');
                end
                
            bytesPerPixel = (info.('uiActualFileLength') - ftell(fid)) / nPixels;
            
            % Read the projection according to the bytes per pixel information
            switch bytesPerPixel
                case (1)
                    M = uint8(fread(fid,nPixels,'uint8'));
                case (2)
                    M = uint16(fread(fid,nPixels,'uint16'));
                case (4)
                    M = uint32(fread(fid,nPixels,'uint32'));
                otherwise
                    fclose(fid);
                    error('ERROR : Incompatible file format. \n');
            end
            
            % Reshape the projection data matrix
            M = reshape(M,info.('uiSizeX'),info.('uiSizeY'));
            
            % Close the file
            fclose(fid);
            
            return
        end
        
        
        
        % Search the selected folder for projection files. 
        function updateProjectionsDir(app, browse)
            % BC: This function updates the kv imaging parameters based on the contents
            % of the projection files.
            
            % Reset the projections list variable
            app.Projections = {};
            % if ~isfield(app.paths, 'projections') || ~ischar(app.paths.projections)
            %     % app.paths.projections = [app.paths.master, '\', app.folders.images, '\',app.PatientDropDown.Value];
            %     app.paths.projections = [app.paths.master, '\', app.folders.images, '\', app.PatientDropDown.Value, '\', app.folders.default_projections];
            % end
            % If the user uses the browse function, then open a UI for
            % folder selection
            if browse
                app.paths.projections = "";
                app.paths.projections = uigetdir(app.paths.projections, ...
                    'Select the folder containing the intrafraction CBCT images (.tiff, .xim, .hnc, .hnd, .dcm, or .his)');
            
                if isequal(app.paths.projections, 0)
                    return;
                else
                    app.paths.persistent = app.paths.projections;
            
                    pathParts = strsplit(app.paths.projections, filesep);
                    fxIdx = find(~cellfun(@isempty, regexp(pathParts, '^Fx\d+$')), 1, 'last');
                    
                    if ~isempty(fxIdx) && ismember(pathParts{fxIdx}, app.FractionDropDown.Items)
                        app.FractionDropDown.Value = pathParts{fxIdx};
                    end
                end
            else
                app.paths.projections = '';
                for i = 1:numel(app.folders.default_projections_options)
                    folder = app.folders.default_projections_options(i);
                    folderpath = fullfile(app.paths.master, app.folders.images, app.PatientDropDown.Value, app.FractionDropDown.Value, folder);
                    if isfolder(folderpath)
                        app.folders.default_projections = folderpath;
                        app.paths.projections = convertStringsToChars(app.folders.default_projections);
                        break;
                    end
                end

                if ~isfield(app.paths, "projections")
                    basePath = fullfile(app.folders.images, app.PatientDropDown.Value, app.FractionDropDown.Value);
                    fullPaths = fullfile(basePath, app.folders.default_projections_options);
                    message = ['Please browse for projection images, could not find within the default folders: ', fullPaths];
                    uiconfirm(app.ContourAlignmentToolUIFigure, message, 'Projection Images Not Found', "Options", "OK", "DefaultOption", 1, "CancelOption", 1);
                    return
                end
            end

            % Create a progress box
            d = uiprogressdlg(app.ContourAlignmentToolUIFigure,'Title','Data Processing',...
                'Message','Loading intrafraction images',...
                'Value',0.2);
            
            % Set the tooltip for the label to the full directory
            app.ProjectionsLabel.Tooltip = app.paths.projections;         
            
            if app.ExportLabel.FontColor(3) ~= 0
                app.paths.export = fullfile(app.paths.projections, 'Contours');
                app.ExportLabel.Tooltip = app.paths.export;
            end

            % Determine file type
            % Determine parameters from the file headers
            if any(size(dir([app.paths.projections '/*.tiff' ]),1))
                app.fileType = '.tiff';
                app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [768 1024]); 
                [app.MatrixSizeLamp.Color] = deal([0.31,0.80,0.00]);
                [app.PixelSpacingLamp.Color,app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([1.00,0.41,0.16]);
                app.parametersWarning.Text = 'Warning: unable to determine the pixel spacing, SID, SDD, and x-offset. Confirm these values before proceeding.';
                app.parametersWarning.Visible = 'on';
                
            elseif any(size(dir([app.paths.projections '/*.xim' ]),1))
                app.fileType = '.xim';
                app.MachineDropDown.Value = "Varian";
                app.CollimatorCassetteDropDown.Visible = 'off';
                app.CollimatorCassetteDropDownLabel.Visible = 'off';
                app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [768 1024]); 
                files = dir([app.paths.projections '/*.xim' ]);
                info = XimReader(app,fullfile(app.paths.projections,files(1).name));
                
                if strcmp(app.ImagingTypeDropDown.Value,'Kilovoltage')
                    app.SIDvalue.Value = round(info.properties.KVSourceVrt*10);
                    app.SDDvalue.Value = round(abs(info.properties.KVDetectorVrt*10)+info.properties.KVSourceVrt*10);
                    app.offsetValue.Value = round(abs(info.properties.KVDetectorLat*10));
                    
                else
                    app.SIDvalue.Value = round(info.properties.MVSourceVrt*10);
                    app.SDDvalue.Value = round(abs(info.properties.MVDetectorVrt*10)+info.properties.MVSourceVrt*10);
                    app.offsetValue.Value = round(abs(info.properties.MVDetectorLat*10));
                    
                end
                
                app.PixelSpacingValue.Value = info.properties.PixelHeight*10;
                [app.MachineDropDownLamp.Color, app.MatrixSizeLamp.Color, app.PixelSpacingLamp.Color,app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([0.31,0.80,0.00]);
                
            elseif any(size(dir([app.paths.projections '/*.hnc' ]),1))
                app.fileType = '.hnc';
                app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [768 1024]); 
                [app.MatrixSizeLamp.Color] = deal([0.31,0.80,0.00]);
                files = dir([app.paths.projections '/*.hnc' ]);
                [info, ~] = HncReader(app,fullfile(app.paths.projections,files(1).name));
                app.PixelSpacingValue.Value = info.dImageResolutionX*10;
                [app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([1.00,0.41,0.16]);
                app.PixelSpacingLamp.Color = [0.31,0.80,0.00];
                app.parametersWarning.Text = 'Warning: unable to determine the SID, SDD, and x-offset. Confirm these values before proceeding.';
                app.parametersWarning.Visible = 'on';
                
            elseif any(size(dir([app.paths.projections '/*.hnd' ]),1))
                app.fileType = '.hnd';
                app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [768 1024]); 
                [app.MatrixSizeLamp.Color] = deal([0.31,0.80,0.00]);
                [app.PixelSpacingLamp.Color,app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([1.00,0.41,0.16]);
                app.parametersWarning.Text = 'Warning: unable to determine the pixel spacing, SID, SDD, and x-offset. Confirm these values before proceeding.';
                app.parametersWarning.Visible = 'on';
                
            elseif any(size(dir([app.paths.projections '/*.his' ]),1))
                app.fileType = '.his';
                app.MachineDropDown.Value = "Elekta";
                app.CollimatorCassetteDropDown.Visible = 'on';
                app.CollimatorCassetteDropDown.Visible = 'on';
                app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [512 512]); 
                [app.MachineDropDownLamp.Color, app.MatrixSizeLamp.Color] = deal([0.31,0.80,0.00]);
                app.paths.frames = [app.paths.projections,'/_Frames.xml'];
                files = dir([app.paths.projections '/*.his' ]);
                [info, ~] = HisReader(app,fullfile(app.paths.projections,files(1).name));
                app.PixelSpacingValue.Value = info.PixelSpacingX;
                [app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([1.00,0.41,0.16]);
                app.PixelSpacingLamp.Color = [0.31,0.80,0.00];
                app.parametersWarning.Text = 'Warning: unable to determine the SID, SDD, and x-offset. Confirm these values before proceeding.';
                app.parametersWarning.Visible = 'on';
                app.InvertIntensityMenu.Checked = 0;
            
            elseif any(size(dir([app.paths.projections '/*.dcm']), 1))
                app.fileType = '.dcm';

                % Create the temp folder
                app.paths.temp = fullfile(app.paths.export, 'temp');
                mkdir(app.paths.temp)

                [app.PixelSpacingLamp.Color,app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([1.00,0.41,0.16]);
                app.parametersWarning.Text = 'Warning: unable to determine the pixel spacing, SID, SDD, and x-offset. Confirm these values before proceeding.';
                app.parametersWarning.Visible = 'on';       
                
                dcmFiles = dir(fullfile(app.paths.projections, '*.dcm'));
                filePath = fullfile(dcmFiles(1).folder, dcmFiles(1).name);
                
                info = dicominfo(filePath);
                            
                % --- Matrix size ---
                if isfield(info, 'Rows') && isfield(info, 'Columns')
                    app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [info.Rows, info.Columns]);
                else
                    app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [768 1024]);  % fallback
                end

                % --- Pixel Spacing ---
                if isfield(info, 'ImagePlanePixelSpacing')
                    spacing = double(info.ImagePlanePixelSpacing);  % [row, col]
                    app.PixelSpacingValue.Value = spacing(1);
                    app.PixelSpacingLamp.Color = [0, 1, 0];  % green
                else
                    app.PixelSpacingLamp.Color = [1.00, 0.41, 0.16];  % orange fallback
                end

                % --- SDD (Source to Detector Distance) ---
                if isfield(info, 'RTImageSID')
                    app.SDDvalue.Value = round(double(info.RTImageSID));  % in mm
                    app.SDDLamp.Color = [0, 1, 0];  % green
                end
                
                % --- SID (Source to Isocenter Distance) ---
                if isfield(info, 'RadiationMachineSAD') && isfield(info, 'TableTopVerticalPosition')
                    sid = double(info.RadiationMachineSAD);
                    app.SIDvalue.Value = round(sid);  % ← this is SID, so goes to SIDvalue field if that's used for SSD
                    app.SIDLamp.Color = [0, 1, 0];
                end

                % --- Offset (lateral) ---
                if isfield(info, 'RTImagePosition')
                    offset = abs(info.RTImagePosition(1));  % mm
                    app.offsetValue.Value = round(offset);
                    app.offsetLamp.Color = [0, 1, 0];
                end

            else
                app.ProjectionsLabel.Text = 'No intrafraction images detected';
                app.ProjectionsLabel.FontColor = [1.00,0.41,0.16];
                app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [768 1024]);
                [app.MatrixSizeLamp.Color] = deal([0.31,0.80,0.00]);
                [app.MachineDropDownLamp.Color, app.PixelSpacingLamp.Color,app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([0.90,0.90,0.90]);
                app.parametersWarning.Visible = 'off';
                allFilesSelected(app)

                % No projections were found. The app remains in prospective selection mode.
                if app.mode == "selection"
                    app.isRetro = false;
                end

                return % exit if no projection files found
            end

            % Projections have been located, we're performing retrospective patient selection.
            if app.mode == "selection"
                app.isRetro = true;
            end
            
            % Update dialogue box
            pause(1)
            d.Value = 1;
            pause(0.5)
            
            % Display the number of projections detected

            if strcmp(app.fileType, '.dcm')
                dcmFiles = dir(fullfile(app.paths.projections, '*.dcm'));
                app.Projections = [];
            
                for k = 1:length(dcmFiles)
                    filePath = fullfile(dcmFiles(k).folder, dcmFiles(k).name);
                    info = dicominfo(filePath);
                    img = dicomread(filePath);
            
                    if isfield(info, 'NumberOfFrames')
                        numFrames = info.NumberOfFrames;
                    else
                        numFrames = 1;
                    end
                    for f = 1:numFrames
                        if numFrames == 1
                            frame = img;
                        else
                            frame = img(:, :, f);
                        end
            
                        % Save image frame
                        imgFilename = sprintf('RI_%04d_frame%02d.mat', k, f);
                        imgPath = fullfile(app.paths.temp, imgFilename);
                        save(imgPath, 'frame');
            
                        % Save header
                        headerFilename = sprintf('RI_%04d_frame%02d_header.mat', k, f);
                        headerPath = fullfile(app.paths.temp, headerFilename);
                        save(headerPath, 'info');
            
                        % Construct projection reference (same structure as other formats)
                        ref.ImagePath = imgPath;
                        ref.HeaderPath = headerPath;
                        ref.SourceDICOM = filePath;
                        ref.FrameIndex = f;
                        ref.name = imgFilename;
                        ref.folder = app.paths.temp;  % simulate expected structure
            
                        % Append to projections
                        app.Projections = [app.Projections; ref];
                    end
                end
            else
                [app.Projections]  = dir([app.paths.projections,'/*' app.fileType]);
            end
      
            app.ProjectionsLabel.Text = [num2str(length(app.Projections)),' files discovered (', app.fileType(2:end),')'];
            app.ProjectionsLabel.FontColor = [0.31,0.80,0.00];
            
            % Update the number of projections to load option
            app.NumberProj.Limits = [1 length(app.Projections)];
            if app.AllCheckBox.Value
                app.NumberProj.Value = length(app.Projections);
            end

            % Hide start/stop angle fields.
            if app.mode == "selection"
                app.StartAngleLabel.Visible = 'off';
                app.StartAngleLamp.Visible = 'off';
                app.StartAngleValue.Visible = 'off';
                app.StopAngleLabel.Visible = 'off';
                app.StopAngleLamp.Visible = 'off';
                app.StopAngleValue.Visible = 'off';
            end

            allFilesSelected(app)
            close(d)
        end

        
        % Search the selected folder for the CT, RS and RP DICOMS
        function updateDICOMDir(app,browse)
            
            % Reset the paths and dcm variables
            app.paths.plan = char();
            app.paths.structure = char();
            app.dcmHeaders = {};
            
            if ~browse
                % Look for default folders.
                folders = { dir(fullfile(app.paths.master, app.folders.plans, app.PatientDropDown.Value)).name};
                for f = app.folders.default_ct_options
                    if any(folders == f)
                        app.folders.default_ct = char(f);
                    end
                end
                for f = app.folders.default_rtplan_options
                    if any(folders == f)
                        app.folders.default_rtplan = char(f);
                    end
                end
                for f = app.folders.default_rtstruct_options
                    if any(folders == f)
                        app.folders.default_rtstruct = char(f);
                    end
                end

                if isfield(app.folders, "default_ct")
                    app.paths.ct = fullfile(app.paths.master, app.folders.plans, app.PatientDropDown.Value, app.folders.default_ct);
                else
                    basePath = fullfile(app.folders.plans, app.PatientDropDown.Value);
                    fullPaths = fullfile(basePath, app.folders.default_ct_options);
                    message = ['Please browse for CT images, could not find within the default folders: ', fullPaths];
                    uiconfirm(app.ContourAlignmentToolUIFigure, message, 'CT Images Not Found', "Options", "OK", "DefaultOption", 1, "CancelOption", 1);
                end
                if isfield(app.folders, "default_rtplan")
                    app.paths.plan = fullfile(app.paths.master, app.folders.plans, app.PatientDropDown.Value, app.folders.default_rtplan);
                else
                    basePath = fullfile(app.folders.plans, app.PatientDropDown.Value);
                    fullPaths = fullfile(basePath, app.folders.default_rtplan_options);
                    message = ['Please browse for RTPLAN file, could not find within the default folders: ', fullPaths];
                    uiconfirm(app.ContourAlignmentToolUIFigure, message, 'RTPLAN File Not Found', "Options", "OK", "DefaultOption", 1, "CancelOption", 1);
                end
                if isfield(app.folders, "default_rtstruct")
                    app.paths.structure = fullfile(app.paths.master, app.folders.plans, app.PatientDropDown.Value, app.folders.default_rtstruct);
                else
                    basePath = fullfile(app.folders.plans, app.PatientDropDown.Value);
                    fullPaths = fullfile(basePath, app.folders.default_rtstruct_options);
                    message = ['Please browse for RTSTRUCT file, could not find within the default folders: ', fullPaths];
                    uiconfirm(app.ContourAlignmentToolUIFigure, message, 'RTSTRUCT File Not Found', "Options", "OK", "DefaultOption", 1, "CancelOption", 1);
                end
            end
            
            % Set the tooltip for the label to the full directory paths
            
            
            

            % --- Collect all DICOM file info ---
            if isfield(app.paths, "ct")
                app.CTLabel.Tooltip = app.paths.ct;
                ctFiles = dir(fullfile(app.paths.ct, '*.dcm'));
            end
            if isfield(app.paths, "rtplan")
                app.PlanLabel.Tooltip = app.paths.plan;
                planFiles = dir(fullfile(app.paths.plan, '*.dcm'));
            end
            if isfield(app.paths, "rtstruct")
                app.StructureLabel.Tooltip = app.paths.structure;
                structFiles = dir(fullfile(app.paths.structure, '*.dcm'));
            end
            if (isfield(app.paths, "ct") && ~isempty(ctFiles)) || ...
                (isfield(app.paths, "rtplan") && ~isempty(planFiles)) || ...
                (isfield(app.paths, "rtstruct") && ~isempty(structFiles))
                d = uiprogressdlg(app.ContourAlignmentToolUIFigure, 'Title', 'Data Processing', ...
                'Message', 'Initializing...', 'Value', 0.01);
            end

            % --- Load CT files ---
            if isfield(app.paths, "ct") && ~isempty(ctFiles)
                d.Message = 'Loading CT DICOM files...';
                for k = 1:length(ctFiles)
                    d.Value = 0.1 + (k/length(ctFiles)) * 0.2;
                    info = dicominfo(fullfile(app.paths.ct, ctFiles(k).name));
                    if strcmp(info.Modality, 'CT')
                        app.dcmHeaders{end+1} = info;
                    end
                end
            end
            
            % --- Load RTPLAN ---
            if isfield(app.paths, "rtplan") && ~isempty(planFiles)
                d.Message = 'Loading Plan DICOM files...';
                for k = 1:length(planFiles)
                    d.Value = 0.4;  % You can fine-tune this if you want smoother animation
                    info = dicominfo(fullfile(app.paths.plan, planFiles(k).name));
                    if strcmp(info.Modality, 'RTPLAN')
                        app.paths.plan = fullfile(app.paths.plan, planFiles(k).name);
                        app.PlanLabel.Tooltip = app.paths.plan;
                        planName = planFiles(k).name;

                        % Get gantry angles for first dynamic beam.
                        beamNames = fieldnames(info.BeamSequence);
                        for i = 1:length(beamNames)
                            beam = info.BeamSequence.(beamNames{i});
                            if beam.BeamType == "DYNAMIC"
                                cpNames = fieldnames(beam.ControlPointSequence);
                                angles = zeros(numel(cpNames), 1);
                                for j = 1:length(cpNames)
                                    angles(j) = beam.ControlPointSequence.(cpNames{j}).GantryAngle;
                                end
                                break;
                            end
                        end

                        % Control point angles are for MV gantry, we need to apply an offset to attain kV imager
                        % angles - depends on the manufacturer.
                        if strcmp(beam.Manufacturer, 'Varian Medical Systems')
                            angles = angles + 90;  % kV panel = MV gantry + 90
                        elseif strcmp(beam.Manufacturer, 'Elekta')
                            angles = angles - 90;  % kV panel = MV gantry - 90
                        else
                            message = ['Could not attain detector angles from RTPLAN file. Unrecognised manufacturer: ', beam.Manufacturer];
                            uialert(app.ContourAlignmentToolUIFigure, message, 'Gantry Offset Error');
                        end

                        % Set start/stop angle.
                        angles = unwrap(angles * pi/180) * 180/pi;
                        if max(angles) > 360
                            angles = angles - 360;
                        elseif min(angles) < -360
                            angles = angles + 360;
                        end
                        app.StartAngleValue.Value = min(angles);
                        app.StopAngleValue.Value = max(angles);
                        [app.StartAngleLamp.Color, app.StopAngleLamp.Color] = deal([0.31,0.80,0.00]);

                        break;
                    end
                end
            end
            
            % --- Load RTSTRUCT ---
            if isfield(app.paths, "rtstruct") && ~isempty(structFiles)
                d.Message = 'Loading Structure Set DICOM files...';
                for k = 1:length(structFiles)
                    d.Value = 0.6;
                    info = dicominfo(fullfile(app.paths.structure, structFiles(k).name));
                    if strcmp(info.Modality, 'RTSTRUCT')
                        app.paths.structure = fullfile(app.paths.structure, structFiles(k).name);
                        app.StructureLabel.Tooltip = app.paths.structure;
                        structureName = structFiles(k).name;
                        break;
                    end
                end
            end

                        
            d.Value = 0.9;   
            
            % Update the labels for the different dicoms
            if ~isempty(app.dcmHeaders)
                app.CTLabel.Text = [num2str(length(app.dcmHeaders)),' CT files discovered'];
                app.CTLabel.FontColor = [0.31,0.80,0.00];
            else
                app.CTLabel.Text = 'No CT files detected';
                app.CTLabel.FontColor = [0.94,0.02,0.02];
            end
            
            if isfile(app.paths.plan)
                app.PlanLabel.Text = ['Plan loaded (' char(planName),')'];
                app.PlanLabel.FontColor = [0.31,0.80,0.00];
            elseif ~browse
                app.PlanLabel.Text = 'Plan not detected';
                app.PlanLabel.FontColor = [0.94,0.02,0.02];
            end
            
            if isfile(app.paths.structure)
                app.StructureLabel.Text = ['Structure set loaded (', char(structureName),')'];
                app.StructureLabel.FontColor = [0.31,0.80,0.00];
                
                try
                    info = dicominfo(app.paths.structure); 
                    info = dicomContours(info);
                catch
                    info = dicominfo(app.paths.structure,'UseVRHeuristic',false);
                    info = dicomContours(info);
                end
                
                ii = 1;
                C = cell(size(info.ROIs,1),1);
                for i = 1:size(info.ROIs,1)
                    if ~isempty(info.ROIs.GeometricType{i,1})>0
                        if strcmp(string(info.ROIs.GeometricType{i,1}(1)),'CLOSED_PLANAR')
                            C(ii,1) = info.ROIs{i,2};
                            ii = ii + 1;
                        end
                    end
                end
                C(ii:end) = [];
                
                app.SelectStructure.Items = [{'Select structure'};C(:,1)];  
                app.SelectStructure.Value = 'Select structure';
                app.SelectStructureStatus.Visible = 'on';
                app.SelectStructureStatus.BackgroundColor = [1.00,0.41,0.16];
                app.SelectStructure.Visible = 'on';
 
            elseif ~browse
                app.StructureLabel.Text = 'Structure set not detected';
                app.StructureLabel.FontColor = [1.00,0.41,0.16];
                app.SelectStructureStatus.BackgroundColor = [1.00,0.41,0.16];
                app.SelectStructure.Visible = 'off';
                app.SelectStructureStatus.Visible = 'off';
            end
            
            if isfile(app.paths.structure) || isfile(app.paths.plan) || ~isempty(app.dcmHeaders)
                d.Value = 1;
                pause(0.5)
                close(d)
            end
            
            allFilesSelected(app)
        end
        

        
        % Determine if all of the required files have been selected.
        % If so, allow the user to proceed and process the data
        function allFilesSelected(app)
            if app.SelectStructureStatus.BackgroundColor(3) == 0 ...
                    && app.PlanLabel.FontColor(3) == 0 ...
                    && app.CTLabel.FontColor(3) == 0 ...
                    && (app.mode == "selection" || app.ProjectionsLabel.FontColor(3) == 0)  % In "selection" mode, you can perform prospective selection - without treatment images.
                
                app.StartProcessing.Enable = 'on';
                
            else
                app.StartProcessing.Enable = 'off';
            end
        end
        
        
        
        % Load the projections and DRRs for the current frame
        function loadImages(app)

            % Load the projection.
            if app.mode == "alignment" || app.isRetro
                if strcmp(app.fileType,'.tiff')
                    app.projection = imread(fullfile(app.Projections(app.currentFrame).folder,app.Projections(app.currentFrame).name));
                    app.projection = app.projection(1:768,1:1024);
                    app.DRR = permute(app.DRRs(:,:,app.currentFrame),[2 1 3]);
                    app.Mask = permute(app.Masks(:,:,app.currentFrame),[2 1 3]);
                    
                elseif strcmp(app.fileType,'.xim')
                    proj_path = fullfile(app.Projections(app.currentFrame).folder,app.Projections(app.currentFrame).name);
                    [~, app.projection] = XimReader(app, proj_path);
                    if any(size(app.projection) == [0, 0])
                        uialert(app.ContourAlignmentToolUIFigure, ['Empty projection found at path: ', proj_path], 'Empty projection')
                        return
                    end
                    app.projection = imrotate(app.projection,-90);
                    app.DRR = permute(app.DRRs(:,:,app.currentFrame),[2 1 3]);
                    app.Mask = permute(app.Masks(:,:,app.currentFrame),[2 1 3]);
                    
                elseif strcmp(app.fileType,'.hnc')
                    [~, app.projection] = HncReader(app,fullfile(app.Projections(app.currentFrame).folder,app.Projections(app.currentFrame).name));
                    app.projection = imrotate(app.projection,-90);
                    app.DRR = permute(app.DRRs(:,:,app.currentFrame),[2 1 3]);
                    app.Mask = permute(app.Masks(:,:,app.currentFrame),[2 1 3]);
                    
                elseif strcmp(app.fileType,'.hnd')
                    [~, app.projection] = HndReader(app,fullfile(app.Projections(app.currentFrame).folder,app.Projections(app.currentFrame).name));
                    app.projection = imrotate(app.projection,90);
                    app.DRR = permute(app.DRRs(:,:,app.currentFrame),[2 1 3]);
                    app.Mask = permute(app.Masks(:,:,app.currentFrame),[2 1 3]);
                    
                elseif strcmp(app.fileType,'.his') 
                    [~, app.projection] = HisReader(app,fullfile(app.Projections(app.currentFrame).folder,app.Projections(app.currentFrame).name));
                    app.projection = imrotate(app.projection,-90);
                    app.DRR = imrotate(app.DRRs(:,:,app.currentFrame),-90);
                    app.Mask = imrotate(app.Masks(:,:,app.currentFrame),-90);
                elseif strcmp(app.fileType, '.dcm')
                    framePath = fullfile(app.Projections(app.currentFrame).folder, ...
                                         app.Projections(app.currentFrame).name);
                    
                    % Load the frame from the .mat file
                    frameData = load(framePath, 'frame');
                    app.projection = frameData.frame;
                            
                    % Load DRR and Mask as in other formats
                    app.DRR = permute(app.DRRs(:,:,app.currentFrame), [2 1 3]);
                    app.Mask = permute(app.Masks(:,:,app.currentFrame), [2 1 3]);
                end
            else
                % Prospective use case doesn't have treatment images.
                app.DRR = permute(app.DRRs(:,:,app.currentFrame),[2 1 3]);
                app.Mask = permute(app.Masks(:,:,app.currentFrame),[2 1 3]);
            end
            
            app.DRR = double(app.DRR);
            app.DRR = (app.DRR - min(app.DRR(:)))/(max(app.DRR(:)) - min(app.DRR(:)));

            % Add 10mm margin to DRR.
            if app.mode == "selection"
                % app.marginPx = floor(app.margin / app.PixelSpacingValue.Value);
                % app.DRR(:, margin_px) = app.marginPx;
                % app.DRR(:, end - margin_px) = app.marginPx;
                % app.DRR(margin_px, :) = app.marginPx;
                % app.DRR(end - margin_px, :) = app.marginPx;
            end
            
            app.Mask = imbinarize(app.Mask,0);
            app.MaskOutline = boundarymask(app.Mask);
    
            if app.mode == "alignment" || app.isRetro
                app.projection = double(app.projection);
                app.projection = log(app.projection + 1);
                app.projection = (app.projection - min(app.projection(:)))/(max(app.projection(:)) - min(app.projection(:)));
            
                if app.InvertIntensityMenu.Checked
                    app.projection = imcomplement(app.projection);
                end
            end
        end
        
        
        
        % Update the display of the images
        function updatePlot(app)
            
            % Update the DRR
            if ~isempty(findall(groot,'Type','figure','Name','DRR Viewer'))
                updateDRR(DRRviewer)
            end
            
            % Select the image on which the contrast adjustment tools
            % operate.
            if app.mode == "alignment" || app.isRetro
                image = app.projection;
            else
                image = app.DRR;
            end
            
            if app.contrastAutoButton.Value || app.contrastManualButton.Value
                
                if strcmp(app.fileType,'.his') 
                    if app.InvertIntensityMenu.Checked
                        lower = prctile(image(:),0);
                        upper = prctile(image(:),90);
                        
                        if app.contrastAutoButton.Value
                            app.LowerSlider.Value = (prctile(image(:),1)-lower)/(upper-lower);
                            app.UpperSlider.Value = (prctile(image(:),80)-lower)/(upper-lower);
                        end

                    else
                        lower = prctile(image(:),10);
                        upper = prctile(image(:),100);
                        
                        if app.contrastAutoButton.Value
                            app.LowerSlider.Value = (prctile(image(:),20)-lower)/(upper-lower);
                            app.UpperSlider.Value = (prctile(image(:),99)-lower)/(upper-lower);
                        end

                    end
                else
                    if app.InvertIntensityMenu.Checked  
                        
                        lower = min(image(:));
                        upper = max(image(:));
%                         lower = prctile(image(:),1);
%                         upper = prctile(image(:),99.9);
                        
                        if app.contrastAutoButton.Value
                            app.LowerSlider.Value = (prctile(image(:),5)-lower)/(upper-lower);
                            app.UpperSlider.Value = (prctile(image(:),99.5)-lower)/(upper-lower);
                        end
                        
                    else
                        lower = min(image(:));
                        upper = max(image(:));
%                         lower = prctile(image(:),0.1);
%                         upper = prctile(image(:),99);
                        if app.contrastAutoButton.Value
                            app.LowerSlider.Value = (prctile(image(:),0.5)-lower)/(upper-lower);
                            app.UpperSlider.Value = (prctile(image(:),95)-lower)/(upper-lower);
                        end
                    end
                end
                

               
            elseif app.contrastROIButton.Value
               lower = app.ROIrange(1);
               upper = app.ROIrange(2);
               
            elseif app.contrastContourButton.Value
                % s = regionprops(double(app.Mask),'BoundingBox');
                % s.BoundingBox = round(s.BoundingBox);
                % 
                % windowsize = 80;
                % windowposition(1) = s.BoundingBox(2)-windowsize/2;
                % if windowposition(1) < 1
                %     windowposition(1) = 1;
                % end
                % 
                % windowposition(2) = s.BoundingBox(2)+s.BoundingBox(4)+windowsize/2;
                % matrixSize = str2num(app.MatrixSizeDropDown.Value);
                % if windowposition(2) > matrixSize(1)
                %     windowposition(2) = matrixSize(1);
                % end
                % 
                % windowposition(3) = s.BoundingBox(1)-windowsize/2;
                % if windowposition(3) < 1
                %     windowposition(3) = 1;
                % end
                % 
                % windowposition(4) =  s.BoundingBox(1)+s.BoundingBox(3)+windowsize/2;
                % if windowposition(4) > matrixSize(2)
                %     windowposition(4) = matrixSize(2);
                % end
                % 
                % lower = min(min(app.projection(windowposition(1):windowposition(2),...
                %     windowposition(3):windowposition(4))));
                % upper = max(max(app.projection(windowposition(1):windowposition(2),...
                %     windowposition(3):windowposition(4))));  
                % Get the region inside the mask
                maskedValues = image(app.Mask > 0);
                
                % Exclude zero values
                maskedValues = maskedValues(maskedValues > 0);
                
                % Compute 3rd percentile and max
                if ~isempty(maskedValues)
                    lower = min(maskedValues);
                    upper = max(maskedValues);
                else
                    % Handle empty mask (fallback values)
                    lower = 0;
                    upper = 1;
                end
                                        
            end
            
            val1 = app.LowerSlider.Value * (upper-lower)+lower;
            val2 = app.UpperSlider.Value * (upper-lower)+lower;
            histogram(app.Histogram,image,100, 'BinLimits',[lower,upper],'EdgeColor','none','FaceColor',[0.65,0.65,0.65]);  
            


            % Set the width of the projection display
            matrixSize = str2num(app.MatrixSizeDropDown.Value);
            if app.UIAxes.Position(3)/app.UIAxes.Position(4) <= matrixSize(2)/matrixSize(1)
                app.displaySize(1) = app.UIAxes.Position(3);
                app.displaySize(2) = app.UIAxes.Position(3)/(matrixSize(2)/matrixSize(1));
            else
                app.displaySize(1) = app.UIAxes.Position(4)*(matrixSize(2)/matrixSize(1));
                app.displaySize(2) = app.UIAxes.Position(4);
            end
            
            % Display the projection and contour
            imshow(image,[min([val1,val2])*max(image(:)) max([val1,val2])*max(image(:))],'Parent',app.UIAxes,'XData', [1 app.displaySize(1)],'YData', [1 app.displaySize(2)]);
            app.UIAxes.XLim = [1 app.displaySize(1)];
            app.UIAxes.YLim = [1 app.displaySize(2)];

            % Add margin for patient selection.
            if app.mode == "selection"
                hold(app.UIAxes, 'on');
                [h, w, ~] = size(image);
                % Map margin from pixels to display.
                marginX = app.marginX * app.displaySize(1) / w;
                marginY = app.marginY * app.displaySize(2) / h;
                rectangle(app.UIAxes, ...
                    'Position', [marginX, marginY, app.displaySize(1) - 2 * marginX, app.displaySize(2) - 2 * marginY], ...
                    'EdgeColor', 'yellow', ...
                    'LineWidth', 1 );
            end
            
            if app.dispContour
                hold(app.UIAxes,'on')
                    maskimage = imshow(app.colour,'Parent',app.UIAxes,'XData', [1 app.displaySize(1)],'YData', [1 app.displaySize(2)]);
                hold(app.UIAxes,'off')
                if app.ContourFillMenu.Checked
                    set(maskimage, 'AlphaData', app.Mask*0.25) 
                else
                    set(maskimage, 'AlphaData', app.MaskOutline) 
                end
            end
            
            % Update the contour centroid location
            if app.mode == "alignment"
                s = regionprops(double(app.Mask),'centroid');
                if ~isempty(s)
                    app.position.Text = sprintf('(%.0f, %.0f)',s(1).Centroid(1),matrixSize(1) - s(1).Centroid(2));
                else
                    app.position.Text = '';
                end
            end
        end
        
        
        
        % Update the the confidence label of the current image
        function save(app)
            if app.mode == "selection"
                return;
            end

            % Get current name
            current = sprintf('Image %d (%.2f%c)',app.currentFrame,app.Projections(app.currentFrame).angle,char(176));
            
            % Get the new confidence value
            if app.C5Button.Value == 1
                nodeidx = 5;
            elseif app.C4Button.Value == 1
                nodeidx = 4;
            elseif app.C3Button.Value == 1
                nodeidx = 3;
            elseif app.C2Button.Value == 1
                nodeidx = 2;
            elseif app.C1Button.Value == 1
                nodeidx = 1;
            else
                nodeidx = 0;
            end
            
            % If the confidence value has changed, update the tree
            if app.Projections(app.currentFrame).confidence ~= nodeidx
            
                % Remove the current node 
                % Update the label of the group to display the number of images
                if app.Projections(app.currentFrame).confidence == 0
                    idx = find(strcmp({app.C0Node.Children.Text}, current));
                    app.C0Node.Children(idx).delete
                    app.C0Node.Text = sprintf('0: Unlabelled (%d images)',size(app.C0Node.Children,1));
                elseif app.Projections(app.currentFrame).confidence == 1
                    idx = find(strcmp({app.C1Node.Children.Text}, current));
                    app.C1Node.Children(idx).delete
                    app.C1Node.Text = sprintf('1: Not at all confident (%d images)',size(app.C1Node.Children,1));
                elseif app.Projections(app.currentFrame).confidence == 2
                    idx = find(strcmp({app.C2Node.Children.Text}, current));
                    app.C2Node.Children(idx).delete
                    app.C2Node.Text = sprintf('2: Not very confident (%d images)',size(app.C2Node.Children,1));
                elseif app.Projections(app.currentFrame).confidence == 3
                    idx = find(strcmp({app.C3Node.Children.Text}, current));
                    app.C3Node.Children(idx).delete
                    app.C3Node.Text = sprintf('3: Neither (%d images)',size(app.C3Node.Children,1));
                elseif app.Projections(app.currentFrame).confidence == 4
                    idx = find(strcmp({app.C4Node.Children.Text}, current));
                    app.C4Node.Children(idx).delete
                    app.C4Node.Text = sprintf('4: Fairly confident (%d images)',size(app.C4Node.Children,1));
                elseif app.Projections(app.currentFrame).confidence == 5
                    idx = find(strcmp({app.C5Node.Children.Text}, current));
                    app.C5Node.Children(idx).delete
                    app.C5Node.Text = sprintf('5: Very confident (%d images)',size(app.C5Node.Children,1));
                end
                
                % Add a new node to the labelled confidence score
                % Update the label of the group to display the number of images
                if app.C5Button.Value == 1
                    uitreenode(app.C5Node,"Text", current); 
                    app.C5Node.Text = sprintf('5: Very confident (%d images)',size(app.C5Node.Children,1));
                elseif app.C4Button.Value == 1
                    uitreenode(app.C4Node,"Text", current); 
                    app.C4Node.Text = sprintf('4: Fairly confident (%d images)',size(app.C4Node.Children,1));
                elseif app.C3Button.Value == 1
                    uitreenode(app.C3Node,"Text", current); 
                    app.C3Node.Text = sprintf('3: Neither (%d images)',size(app.C3Node.Children,1));
                elseif app.C2Button.Value == 1
                    uitreenode(app.C2Node,"Text", current); 
                    app.C2Node.Text = sprintf('2: Not very confident (%d images)',size(app.C2Node.Children,1));
                elseif app.C1Button.Value == 1
                    uitreenode(app.C1Node,"Text", current);
                    app.C1Node.Text = sprintf('1: Not at all confident (%d images)',size(app.C1Node.Children,1));
                else
                    uitreenode(app.C0Node,"Text", current); 
                    app.C0Node.Text = sprintf('0: Unlabelled (%d images)',size(app.C0Node.Children,1));
                end
                
                % Update the stored confidence 
                app.Projections(app.currentFrame).confidence = nodeidx;
                
                % Go through the updated node and rename everything so it is in order
                i = 1;
                for k1 = 1:length(app.Projections)
                    if app.Projections(k1).confidence == nodeidx
                        app.Tree.Children(nodeidx + 1).Children(i).Text = sprintf('Image %d (%.2f%c)',k1,app.Projections(k1).angle,char(176));
                        i = i + 1;
                    end
                end
            end
            
            % Update the mask
            if strcmp(app.fileType,'.his')
                app.Masks(:,:,app.currentFrame) = imrotate(app.Mask,90);
            else
                app.Masks(:,:,app.currentFrame) = permute(app.Mask,[2 1 3]);
            end 
        end
        
        
        
        % Update the the confidence label of the current image
        function updateImageDetails(app)

            % Update the current image number
            app.projectionnumber.Text = num2str(app.currentFrame);

            if app.mode == "selection"
                return;
            end
            
            % Set the confidence
            if app.Projections(app.currentFrame).confidence == 5
                app.C5Button.Value = 1;
            elseif app.Projections(app.currentFrame).confidence == 4
                app.C4Button.Value = 1;
            elseif app.Projections(app.currentFrame).confidence == 3
                app.C3Button.Value = 1;
            elseif app.Projections(app.currentFrame).confidence == 2
                app.C2Button.Value = 1;
            elseif app.Projections(app.currentFrame).confidence == 1
                app.C1Button.Value = 1;
            else
                app.C0Button.Value = 1;
            end
        end

        
        
        % Allow the user to drag and drop the contour
        function moveContour(app,~,~)
            if app.mode == "selection"
                return;
            end
            
            % Get the centroid of the contour
            s = regionprops(app.Mask,'centroid');
            centroids = cat(1,s.Centroid);
            
            % Calculate the difference between the pointer position and the
            % centroid
            matrixSize = str2num(app.MatrixSizeDropDown.Value);
            xdifference = round((app.UIAxes.CurrentPoint(1,1)/app.displaySize(1)*matrixSize(2)) - mean(centroids(:,1)));
            ydifference = round((app.UIAxes.CurrentPoint(1,2)/app.displaySize(2)*matrixSize(1)) - mean(centroids(:,2)));
            
            % Move contour up
            if ydifference < 0
                app.MaskOutline(1:abs(ydifference),:) = [];
                app.MaskOutline(end+abs(ydifference),:) = 0;
                app.Mask(1:abs(ydifference),:) = [];
                app.Mask(end+abs(ydifference),:) = 0;  
                
            % Move contour down
            elseif ydifference > 0
                blank = zeros(size(app.MaskOutline));
                app.MaskOutline(end-ydifference:end,:) = [];
                blank(ydifference+2:end,:) = app.MaskOutline;
                app.MaskOutline = blank;
    
                blank = zeros(size(app.Mask));
                app.Mask(end-ydifference:end,:) = [];
                blank(ydifference+2:end,:) = app.Mask;
                app.Mask = blank;  
            end
            
            % Move contour left
            if xdifference < 0
                app.MaskOutline(:,1:abs(xdifference)) = [];
                app.MaskOutline(:,end+abs(xdifference)) = 0;
                app.Mask(:,1:abs(xdifference)) = [];
                app.Mask(:,end+abs(xdifference)) = 0;
            
            % Move contour right
            elseif xdifference > 0
                blank = zeros(size(app.MaskOutline));
                app.MaskOutline(:,end-xdifference:end) = [];
                blank(:,xdifference+2:end) = app.MaskOutline;
                app.MaskOutline = blank;
    
                blank = zeros(size(app.Mask));
                app.Mask(:,end-xdifference:end) = [];
                blank(:,xdifference+2:end) = app.Mask;
                app.Mask = blank;
            end

            updatePlot(app)
        end
        
        
        
        % Export the contours as .png files
        function exportContours(app)
            
            save(app)

            % Check if the export folder already contains contours. If so
            % allow the user to select a new location and overwrite the
            % files.
            if any(size(dir([app.paths.export '/*.png' ]),1))
                msg = 'The export folder already contains contours. Proceeding will overwrite these files.';
                title = 'Confirm Export';
                selection = uiconfirm(app.ContourAlignmentToolUIFigure,msg,title, ...
                       'Options',{'Overwrite','Choose new location','Cancel'}, ...
                       'DefaultOption',1,'CancelOption',3);  
                
                switch selection
                    case 'Overwrite'
                        delete([app.paths.export,'/*.png'])
                    
                    case 'Choose new location'
                        
                        app.paths.export = uigetdir(app.paths.persistent,'Select the export folder location');
                
                        if ~isequal(app.paths.export,0)
                            app.paths.persistent = app.paths.export;
                            app.paths.export = fullfile(app.paths.export, 'Contours');
                            mkdir(app.paths.export) 
                        end
                        
                    case 'Cancel'
                        return
                end
                
            end

            % Create a progress box 
            d = uiprogressdlg(app.ContourAlignmentToolUIFigure,'Title','Export',...
                    'Message','Exporting contours');
            
            % Calculate the distance at isocentre.
            isoSize = app.SIDvalue.Value/app.SDDvalue.Value * app.PixelSpacingValue.Value;
            file = fopen(fullfile(app.paths.export,'contour_shifts.csv'), 'w');
            fprintf(file, 'file,u-direction(mm),v-direction(mm)\n');
            
            % Export each contour
            for k1 = 1:length(app.Projections)
               
               d.Value = (k1/length(app.Projections));
               
               name = app.Projections(k1).name;
               k = strfind(name,'.');
               
                if app.Projections(k1).confidence == 0
                    baseFileName = [name(1:k(end)-1),'_mask_c0.png'];
                elseif app.Projections(k1).confidence == 1
                    baseFileName = [name(1:k(end)-1),'_mask_c1.png'];
                elseif app.Projections(k1).confidence == 2
                    baseFileName = [name(1:k(end)-1),'_mask_c2.png'];
                elseif app.Projections(k1).confidence == 3
                    baseFileName = [name(1:k(end)-1),'_mask_c3.png'];
                elseif app.Projections(k1).confidence == 4
                    baseFileName = [name(1:k(end)-1),'_mask_c4.png'];
                elseif app.Projections(k1).confidence == 5
                    baseFileName = [name(1:k(end)-1),'_mask_c5.png'];                 
                end
                
                
                if strcmp(app.fileType,'.his')
                    exportMask = imrotate(app.Masks(:,:,k1),-90);
                else
                    exportMask = permute(app.Masks(:,:,k1),[2 1 3]);
                end

                exportMask = imbinarize(exportMask,0);
                
                
                fullFileName = fullfile(app.paths.export, baseFileName);
                imwrite(exportMask, fullFileName);
                
                
                % Calculate the contour shifts
                s1 = regionprops(double(exportMask),'centroid');
                
                if strcmp(app.fileType,'.his')
                    s2 = regionprops(double(imbinarize(imrotate(app.originalMasks(:,:,k1),-90),0)),'centroid');
                else
                    s2 = regionprops(double(imbinarize(permute(app.originalMasks(:,:,k1),[2 1 3]),0)),'centroid');
                end                                
                
                matrixSize = str2num(app.MatrixSizeDropDown.Value);
                s1.Centroid(2) = matrixSize(1)-s1.Centroid(2);
                s2.Centroid(2) = matrixSize(1)-s2.Centroid(2);
                diff= (s1.Centroid-s2.Centroid)*isoSize;
                fprintf(file, '%s,%.4f,%.4f\n', name(1:k(end)-1),diff(1),diff(2));
            end  
            
            fclose(file);
            
            % Export the parameters to a text file
            file = fopen(fullfile(app.paths.export,'params.txt'), 'w');
            fprintf(file, 'Contour Alignment Tool v%s\n',app.version);
            fprintf(file, 'Pixel Spacing: %.3f\n', app.PixelSpacingValue.Value);
            fprintf(file, 'SID: %.2f\n', app.SIDvalue.Value);
            fprintf(file, 'SDD: %.2f\n', app.SDDvalue.Value);
            fprintf(file, 'Detector X Offset: %.2f\n', app.offsetValue.Value);
            fclose(file);
            
           
            close(d)
           
            msg = sprintf('%d contours succesfully exported to %s',length(app.Projections), app.paths.export);
            uiconfirm(app.ContourAlignmentToolUIFigure,msg,'Export Complete', ...
                   'Options',{'Ok'});    
        end
        

        % Reset all variables for a new session.
        function newSession(app,type)
        % BC: Runs when user selects new fraction/patient.
            % Reset to prospective patient.
            if app.mode == "selection"
                app.isRetro = false;
                disp("setting pros");

                % Show start/stop angles.
                app.StartAngleLabel.Visible = 'on';
                app.StartAngleLamp.Visible = 'on';
                app.StartAngleValue.Visible = 'on';
                app.StopAngleLabel.Visible = 'on';
                app.StopAngleLamp.Visible = 'on';
                app.StopAngleValue.Visible = 'on';
            end
            
            % Disable all contour alignment tools
            app.NavigationPanel.Enable = 'off';
            if app.mode == "alignment"
                app.ContourAlignmentPanel.Enable = 'off';
            end
            app.ContrastAdjustmentPanel.Enable = 'off';
            app.UIAxes.Visible = 'off';
            app.ContourColourMenu.Enable = 'off';
            app.ContourFillMenu.Enable = 'off';
            app.DRRViewerMenu.Enable = 'off'; 
            app.ExportMenu.Enable = 'off';
            app.ExportAsMenu.Enable = 'off';
            app.NewFractionMenu.Enable = 'off';
            app.NewPatientMenu.Enable = 'off';
            app.InvertIntensityMenu.Enable = 'off';
            app.QuickExportMenu.Visible = 'off';
            
            % Reset the projections list
            if app.mode == "alignment"
                app.C0Button.Value = 1;
                app.C0Node.Children.delete;
                app.C1Node.Children.delete;
                app.C2Node.Children.delete;
                app.C3Node.Children.delete;
                app.C4Node.Children.delete;
                app.C5Node.Children.delete;
            elseif app.mode == "selection"
                app.PassedNode.Children.delete;
                app.FailedNode.Children.delete;
            end
            
            % Reset contour alignment tools to defaults
            delete(app.DrrApp)
            cla(app.Histogram)
            cla(app.UIAxes)
            app.UpperSlider.Value = 1;
            app.LowerSlider.Value = 0;
            app.contrastManualButton.Value = 0;

            % set window level based on contours by default
            % zx
            % app.contrastAutoButton.Value = 1;
            app.contrastAutoButton.Value = 0;
            % app.contrastContourButton.Value = 0;
            app.contrastContourButton.Value = 1;
            %/zx

            app.contrastROIButton.Value = 0;
            
            % Enable all data processing components
            app.DataProcessingPanel.Visible = 'on';
            app.ProjectionsBrowse.Enable = 'on';
            app.StructureBrowse.Enable = 'on';
            app.CTBrowse.Enable = 'on';
            app.PlanBrowse.Enable = 'on';
            app.MasterBrowse.Enable = 'on';
            app.kVImagingParametersPanel.Enable = 'on';
            app.SelectStructure.Enable = 'on';
            
            if isfield(app.paths, 'master')
                app.PatientDropDown.Enable = 'on';
                
                app.FractionDropDown.Value = 'Select fraction';
                if type == 1
                    app.FractionDropDown.Enable = 'on';
                end
            end
            
            app.ProjectionsLabel.Text = 'Intrafraction images folder not selected';
            app.ProjectionsLabel.FontColor = [1.00,0.41,0.16];
            app.ProjectionsLabel.Tooltip = '';
            
            app.ExportLabel.Text = '(optional)';
            app.ExportLabel.FontColor = [0.502 0.502 0.502];
            app.ExportLabel.Tooltip = '';
            
            
            if strcmp(app.fileType,'.his')
                app.offsetValue.Value = -1*app.offsetValue.Value;
            end
            
            rmdir(app.paths.temp,'s');
            
            % If the user is selecting a new patient reset dicoms
            if type == 2
                
                if isfield(app.paths, 'master')
                    app.PatientDropDown.Value = 'Select patient';  
                end
                
                app.CTLabel.Text = 'CT folder not selected';
                app.CTLabel.FontColor = [1.00,0.41,0.16];
                app.CTLabel.Tooltip = '';
                
                app.PlanLabel.Text = 'Plan file not selected';
                app.PlanLabel.FontColor = [1.00,0.41,0.16];
                app.PlanLabel.Tooltip = '';
                
                app.StructureLabel.Text = 'Structure set file not selected';
                app.StructureLabel.FontColor = [1.00,0.41,0.16];
                app.SelectStructureStatus.BackgroundColor = [1.00,0.41,0.16];
                app.SelectStructure.Visible = 'off';
                app.SelectStructureStatus.Visible = 'off';
                app.StructureLabel.Tooltip = '';
            end
            
            [app.MachineDropDownLamp.Color, app.MatrixSizeLamp.Color, app.PixelSpacingLamp.Color,app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color,app.StartAngleLamp.Color, app.StopAngleLamp.Color] = deal([0.90,0.90,0.90]);
            if app.mode == "selection" && type == 2
                % BC: Don't clear out start/stop angle for new fraction - as the plan is still loaded
                % and this is where start/stop angles come from.
                [app.StartAngleLamp.Color, app.StopAngleLamp.Color] = deal([0.90,0.90,0.90]);
            end
            app.parametersWarning.Visible = 'off';

            allFilesSelected(app);
        end
        
        
        
        % Close the contour alignment tool
        function closeapp(app, ~, event)
            if strcmp(event.SelectedOption,'OK')
                if isfield(app.paths, 'temp')
                    if exist(app.paths.temp,"file")
                        fclose('all');
                        rmdir(app.paths.temp,'s');
                    end
                end

                delete(app.DrrApp)
                delete(app.UpdatesApp)
                delete(app.AboutApp)
                delete(app.HelpApp)
                delete(app)
            end
        end
        
        

    end

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app)
            
            % Get the full path to the .mlapp file
            appRoot = fileparts(mfilename('fullpath'));
        
            % Construct full paths to the subfolders
            addpath(fullfile(appRoot, 'ContourAlignmentTool_resources'));
            addpath(fullfile(appRoot, 'Supporting Apps'));
            addpath(fullfile(appRoot, 'Dependencies'));

            % Get the app mode.
            modePath = fullfile(appRoot, 'mode.txt');
            disp(modePath);
            if isfile(modePath)
                app.mode = strtrim(fileread(modePath));
            end

            % Get the app version.
            versionPath = fullfile(appRoot, 'version.txt');
            disp(versionPath)
            if isfile(versionPath)
                app.version = strtrim(fileread(versionPath));
            end
            
            % Set the UI window name
            disp(app.mode)
            if app.mode == "alignment"
                app.ContourAlignmentToolUIFigure.Name = sprintf('Contour Alignment Tool (v%s)', app.version);
            elseif app.mode == "selection"
                app.ContourAlignmentToolUIFigure.Name = sprintf('Patient Selection Tool (v%s)', app.version);
            end

            % Change the background image.
            app.DirectoriesImage.ImageSource = 'directories.png';

            % Change "image parameters" based on mode.
            if app.mode == "alignment"
                app.MachineDropDown.Visible = 'off';
                app.MachineDropDownLabel.Visible = 'off';
                app.MachineDropDownLamp.Visible = 'off';
                app.StartAngleLabel.Visible = 'off';
                app.StartAngleLamp.Visible = 'off';
                app.StartAngleValue.Visible = 'off';
                app.StopAngleLabel.Visible = 'off';
                app.StopAngleLamp.Visible = 'off';
                app.StopAngleValue.Visible = 'off';
            end
            % Defaults to "Varian", which doesn't have collimator cassettes.
            app.CollimatorCassetteDropDown.Visible = 'off';
            app.CollimatorCassetteDropDownLabel.Visible = 'off';

            % Change the navigation panel items.
            if app.mode == "alignment"
                app.PassedNode.delete;
                app.FailedNode.delete;
            elseif app.mode == "selection"
                app.C0Node.delete;
                app.C1Node.delete;
                app.C2Node.delete;
                app.C3Node.delete;
                app.C4Node.delete;
                app.C5Node.delete;
                app.ContourAlignmentPanel.delete;
            end

            % Disable interactivity
            disableDefaultInteractivity(app.UIAxes)
            disableDefaultInteractivity(app.Histogram)
            
            % Set the first path as the current directory
            app.paths.persistent = pwd;

            % Set variables for monitoring the pointer position
            app.pointerManager.enterFcn = [];
            app.pointerManager.exitFcn  = [];
            app.pointerManager.traverseFcn = @app.moveContour;
        end

        % Menu selected function: AboutMenu, CheckforUpdatesMenu, 
        % ...and 11 other components
        function MenuSelections(app, event)
            % Perform different actions depending on the menu item selected
            switch event.Source.Text
                
                % Open the about software window
                case 'About'
                    app.AboutApp = AboutSoftware(app);
                    
                % Open the instructions window
                case 'Manual'
                    app.HelpApp = Manual(app);
                    
                case 'Keyboard Shortcuts'
                    C = {'Move contour: arrow keys',...
                        'Toggle contour visibility: spacebar',...
                        'Next projection: >',...
                        'Previous projection: <',...
                        'Alignment confidence: numeric keys',...
                        'DRR viewer: Ctrl + D',...
                        'Export: Ctrl + E',...
                        'New patient: Ctrl + P',...
                        'New fraction: Ctrl + F',...
                        'Quit: Ctrl + Q',...
                        };
                    message = sprintf('%s\n',C{:});
                    uialert(app.ContourAlignmentToolUIFigure,message,"Keyboard Shortcuts Summary", ...
                        "Icon","info");
                  
                % Open the new features window
                case "Check for Updates"
                    app.UpdatesApp = CheckForUpdates(app);
                  
                % Generate a draft email for reporting an issue
                case "Report an Issue..."
                    web('https://github.com/Image-X-Institute/contour-alignment-tool/issues')
                   
                % Open the DRR viewer
                case 'DRR Viewer'
                    app.DrrApp = DRRviewer(app); 
                    updatePlot(app)
                 
                % Create a new session for the current patient
                case 'New Fraction...'
                    newSession(app,1)
                  
                % Create a new session for a new patient 
                case 'New Patient...'
                    newSession(app,2)
                    
                % Export to the pre-selected location
                case {'Export','  Quick Export  '}
                    exportContours(app);
                  
                % Export to a new location of the user's choice
                case 'Export As...'
                    app.paths.export = uigetdir(app.paths.persistent,'Select the export folder location');
            
                    if ~isequal(app.paths.export,0)
                        app.paths.persistent = app.paths.export;
        
                        exportContours(app)
                    end
                   
                % Change the fill of the contour
                case 'Contour Fill'
                    if app.ContourFillMenu.Checked
                        app.ContourFillMenu.Checked = 0;
                    else
                        app.ContourFillMenu.Checked = 1;
                    end
                
                    updatePlot(app)  
                    
                % Invert the intensity of the projection
                case 'Invert Intensity'
                    if app.InvertIntensityMenu.Checked
                        app.InvertIntensityMenu.Checked = 0;
                    else
                        app.InvertIntensityMenu.Checked = 1;
                    end

                    app.projection = imcomplement(app.projection);
                    
                    lower =  app.LowerSlider.Value;
                    app.LowerSlider.Value = 1 - app.UpperSlider.Value;
                    app.UpperSlider.Value = 1 - lower;
                    
                    histogram(app.Histogram,app.projection, 50);
                    updatePlot(app) 
            end
        end

        % Button pushed function: MasterBrowse
        function automatedSearch(app, event)
            % List the patient folders within the selected master folder
            app.paths.master = uigetdir(app.paths.persistent,'Select the clinical trial folder');

            if ~isequal(app.paths.master,0)
                app.paths.persistent = app.paths.master;
            else
                return
            end

            % Look for default folders.
            folders = { dir(app.paths.master).name};
            for f = app.folders.images_options
                if any(folders == f)
                    app.folders.images = char(f);
                end
            end
            for f = app.folders.plans_options
                if any(folders == f)
                    app.folders.plans = char(f);
                end
            end
                    
            if isfield(app.folders, "plans")
                patients = dir(fullfile(app.paths.master, app.folders.plans));
                dirFlags = [patients.isdir];
                dirFlags(1:2) = 0;
                patients = patients(dirFlags);
                patients = struct2cell(patients);
                app.PatientDropDown.Items = [{'Select patient'},patients(1,:)]; 
                app.PatientDropDown.Value = 'Select patient'; 
                app.PatientDropDown.Enable = 'on';
            end
        end

        % Callback function
        function refreshFractions(app, event)
            % If the user selects a new patient from the dropdown, load the
            % new list of fraction folders.
            if ~strcmp(app.PatientDropDown.Value,'Select patient')
            
                fractions = dir(fullfile(app.paths.master, app.folders.images, app.PatientDropDown.Value));
                fractions(strcmp({s.name}, "temp")) = [];
                dirFlags = [fractions.isdir];
                dirFlags(1:2) = 0;
                fractions = fractions(dirFlags);
                fractions = struct2cell(fractions);
                app.FractionDropDown.Items = [{'Select fraction'},fractions(1,:)];  
                app.FractionDropDown.Value = 'Select fraction'; 
                
                updateDICOMDir(app,0)
                
                app.FractionDropDown.Enable = 'on';
                
            else
                app.CTLabel.Text = 'CT folder not selected';
                app.CTLabel.FontColor = [1.00,0.41,0.16];
                
                app.PlanLabel.Text = 'Plan file not selected';
                app.PlanLabel.FontColor = [1.00,0.41,0.16];
                
                app.StructureLabel.Text = 'Structure set file not selected';
                app.StructureLabel.FontColor = [1.00,0.41,0.16];
                app.SelectStructure.Visible = 'off';
                
                app.FractionDropDown.Items = {'Fraction'};
                app.FractionDropDown.Enable = 'off';
                
            end
            
            app.ProjectionsLabel.Text = 'Intrafraction images folder not selected';
            app.ProjectionsLabel.FontColor = [1.00,0.41,0.16];
            [app.PixelSpacingLamp.Color,app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([0.90,0.90,0.90]);
            app.parametersWarning.Visible = 'off';
            % 
            % if app.mode == "selection"
            %     app.paths.export = fullfile(app.paths.master, app.folders.images, app.PatientDropDown.Value);
            % end
        end

        % Callback function
        function fractionSelected(app, event)
          
        end

        % Button pushed function: CTBrowse
        function CTBrowsePushed(app, event)
            % Check the selected folder for CT files 
            
            app.paths.ct = uigetdir(app.paths.persistent,'Select the folder containing the CT files');
            
            if ~isequal(app.paths.ct,0)
                app.paths.persistent = app.paths.ct;
                app.CTLabel.Tooltip = app.paths.ct;
                updateDICOMDir(app,1)
            end
                 
        end

        % Button pushed function: PlanBrowse
        function PlanBrowsePushed(app, event)
            % Check if the selected file is a plan dcm
            
            [file,path] = uigetfile('/*.dcm','Select the plan file',...
                app.paths.persistent);
            
            if ~isequal(file,0)
                app.paths.plan = [path file];
                app.paths.persistent = path;  
                app.PlanLabel.Tooltip = app.paths.plan;
            
                try
                    info = dicominfo(app.paths.plan);
                catch 
                    app.PlanLabel.Text = 'Modality of selected file is not RTPLAN';
                    app.PlanLabel.FontColor = [0.94,0.02,0.02];
                    return
                end
                
                if strcmp(info.Modality,'RTPLAN')
                    app.PlanLabel.Text = ['Plan loaded (',file,')'];
                    app.PlanLabel.FontColor = [0.31,0.80,0.00];
                else
                    app.PlanLabel.Text = 'Modality of selected file is not RTPLAN';
                    app.PlanLabel.FontColor = [0.94,0.02,0.02];
                end
                
                allFilesSelected(app)
            
            end
        end

        % Button pushed function: StructureBrowse
        function StructureBrowsePushed(app, event)
            % Check if the selected file is a structure dcm
            
            [file,path] = uigetfile('/*.dcm','Select the structure set file',...
                app.paths.persistent);
            
            d = uiprogressdlg(app.ContourAlignmentToolUIFigure,'Title','Data Processing',...
                'Message','Loading structure set contours',...
                'Value',0);
            
            if ~isequal(file,0)
                app.paths.structure = [path file];
                app.paths.persistent = path;
                app.StructureLabel.Tooltip = app.paths.structure;
            
            
                try
                    info = dicominfo(app.paths.structure);
                catch 
                    app.StructureLabel.Text = 'Modality of selected file is not RTSTRUCT';                   
                    app.StructureLabel.FontColor = [1.00,0.41,0.16];
                    app.SelectStructureStatus.BackgroundColor = [1.00,0.41,0.16];
                    app.SelectStructure.Visible = 'off';
                    app.SelectStructureStatus.Visible = 'off';
                    
                    return
                end
                
                % List the structures if the file is a structure dcm
                if strcmp(info.Modality,'RTSTRUCT')
                    
                    d.Value = 0.2;
                    
                    app.StructureLabel.Text = ['Structure set loaded (' char(file),')'];
                    app.StructureLabel.FontColor = [0.31,0.80,0.00];
                    
                    try
                        info = dicominfo(app.paths.structure); 
                        info = dicomContours(info);
                    catch
                        info = dicominfo(app.paths.structure,'UseVRHeuristic',false);
                        info = dicomContours(info);
                    end
                    
                    ii = 1;
                    C = cell(size(info.ROIs,1),1);
                    for i = 1:size(info.ROIs,1)
                        if ~isempty(info.ROIs.GeometricType{i,1})>0
                            if strcmp(string(info.ROIs.GeometricType{i,1}(1)),'CLOSED_PLANAR')
                                C(ii,1) = info.ROIs{i,2};
                                ii = ii + 1;
                            end
                        end
                    end
                    C(ii:end) = [];
                    
                    app.SelectStructure.Items = [{'Select structure'};C(:,1)];  
                    app.SelectStructureStatus.BackgroundColor = [1.00,0.41,0.16];
                    app.SelectStructureStatus.Visible = 'on';
                    app.SelectStructure.Visible = 'on';
                    
                    d.Value = 1;
                    pause(0.5)
                    close(d)
                    
                else
                    app.StructureLabel.Text = 'Modality of selected file is not RTSTRUCT';                   
                    app.StructureLabel.FontColor = [1.00,0.41,0.16];
                    app.SelectStructureStatus.BackgroundColor = [1.00,0.41,0.16];
                    app.SelectStructure.Visible = 'off';
                    app.SelectStructureStatus.Visible = 'off';
                end
    
                allFilesSelected(app) 
            end
        end

        % Value changed function: SelectStructure
        function StructureSelected(app, event)
            if ~strcmp(app.SelectStructure.Value,'Select structure')
                app.SelectStructureStatus.BackgroundColor = [0.31,0.80,0.00];
            else
                app.SelectStructureStatus.BackgroundColor = [1.00,0.41,0.16];
            end
            allFilesSelected(app)
        end

        % Button pushed function: ProjectionsBrowse
        function ProjectionsBrowsePushed(app, event)
            updateProjectionsDir(app,1)
        end

        % Value changed function: AllCheckBox
        function projectionsLoad(app, event)
            % Change the number of projections to load if the user selects
            % 'All'
            value = app.AllCheckBox.Value;
            if value
                if ~isempty(app.Projections)
                    app.NumberProj.Value = length(app.Projections);
                end
                app.NumberProj.Enable = 'off';
            else
                app.NumberProj.Enable = 'on';
            end
        end

        % Button pushed function: ExportBrowse
        function ExportBrowsePushed(app, event)
            % Set the export folder
            
            app.paths.export = uigetdir(app.paths.persistent,'Select the export folder location');
            
            if ~isequal(app.paths.export,0)
                app.paths.persistent = app.paths.export;
                app.paths.export = fullfile(app.paths.export, 'Contours');
                
                app.ExportLabel.Tooltip = app.paths.export;
                app.ExportLabel.Text = 'Export folder selected';
                app.ExportLabel.FontColor = [0.31,0.80,0.00];
            end
        end

        % Button pushed function: StartProcessing
        function StartProcessingButtonPushed(app, event)
        % BC: This function creates the DRRs.
            try

                % Disable all of the user selection buttons
                app.ProjectionsBrowse.Enable = 'off';
                app.StructureBrowse.Enable = 'off';
                app.CTBrowse.Enable = 'off';
                app.PlanBrowse.Enable = 'off';
                app.MasterBrowse.Enable = 'off';
                app.kVImagingParametersPanel.Enable = 'off';
                app.PatientDropDown.Enable = 'off';
                app.FractionDropDown.Enable = 'off';
                app.SelectStructure.Enable = 'off';
                app.StartProcessing.Enable = 'off';
                
                % Create dialogue box
                d = uiprogressdlg(app.ContourAlignmentToolUIFigure,'Title','Data processing in progress',...
                    'Message','Determining projection angles');
                
                % Create the temp folder (execlude stacked dcm projection
                % case where the folder has been created
                app.paths.temp = fullfile(app.paths.export, 'temp');
                if ~exist(app.paths.temp, 'dir')
                    mkdir(app.paths.temp);
                end
                % If the projections are .his file, load the frames.xml
                % file
                if strcmp(app.fileType,'.his') 
                    info = readstruct(app.paths.frames);
                end
                
                % Select a subset of projections 
                if ~isempty(app.Projections)
                    if ~app.AllCheckBox.Value
                        updatedProjections = struct('name',[],'folder',[]);
                        updatedFrames = zeros(1, app.NumberProj.Value);
                        
                        projNum = round(linspace(1,length(app.Projections),app.NumberProj.Value));
        
                        k = 1;
                        for i = 1:app.NumberProj.Value
                            
                            updatedProjections(k).name = app.Projections(projNum(k)).name;
                            updatedProjections(k).folder = app.Projections(projNum(k)).folder;
                            
                            if strcmp(app.fileType,'.his') 
                                updatedFrames(k) = info.Frames.Frame(projNum(k)).GantryAngle;
                            end
                            
                            k = k+1;
                        end
                        
                        app.Projections = updatedProjections;
                    elseif strcmp(app.fileType,'.his') 
                        updatedFrames = zeros(1, app.NumberProj.Value);
                        
                        for i = 1:app.NumberProj.Value
                            updatedFrames(i) = info.Frames.Frame(i).GantryAngle;
                        end
                    end
                end
                            
                % Extract the projection angles from all file names
                file = fopen(fullfile(app.paths.temp,'ProjectionAngles.csv'), 'w');
                
                % Offset the angle for megavoltage images
                if strcmp(app.ImagingTypeDropDown.Value,'Megavoltage')
                    offset = 90;
                else
                    offset = 0;
                end
                
                % Loop through all projections and acquire the angle
                if app.mode == "alignment" || app.isRetro
                    for i = 1:length(app.Projections)
                        
                        if strcmp(app.fileType,'.tiff')
                            % BC: Tiff images take the angle from the filename - and assume this is MV gantry, not always true.
                            % Also are 'tiff' images always from Varian, as we're adding +90.
                            filename = app.Projections(i).name;
                            number_index = find(filename == '_');
                            endIndex = find(filename == '.');
                            projectionAngle = str2double(filename(number_index(3)+1 : endIndex(2)-1));
                            projectionAngle = projectionAngle + 90 + offset;
                        
                        elseif strcmp(app.fileType,'.xim')
                            % BC: Xim images read MV gantry angle from properties - this is probably correct as straight from Varian?
                            info = XimReader(app, fullfile(app.Projections(i).folder, app.Projections(i).name));
                            projectionAngle = info.properties.GantryRtn + 90 - offset; 
                            % BC: Varian TrueBeam detector panel size. Would need to be updated for HyperSight 
                            % with a larger imaging panel.
                            disp(projectionAngle);
                            disp([info.properties.KVCollimatorX1, info.properties.KVCollimatorX2, info.properties.KVCollimatorY1, info.properties.KVCollimatorY2]);
                            
                        elseif strcmp(app.fileType,'.hnc') 
                            info = HncReader(app, fullfile(app.Projections(i).folder, app.Projections(i).name));
                            projectionAngle = info.dCBCTPositiveAngle - 90 - offset; 
                            
                        elseif strcmp(app.fileType,'.hnd') 
                            info = HndReader(app, fullfile(app.Projections(i).folder, app.Projections(i).name));
                            projectionAngle = info.dCBCTPositiveAngle - 90 - offset; 
                            
                        elseif strcmp(app.fileType,'.his') 
                            projectionAngle = updatedFrames(i) - 90 - offset;
                            app.offsetValue.Value = -1*app.offsetValue.Value;
                        
                        elseif strcmp(app.fileType, '.dcm')
                            [~, baseName, ~] = fileparts(app.Projections(i).name);
                            headerName = [baseName, '_header.mat'];
                            headerPath = fullfile(app.Projections(i).folder, headerName);
                            
                            % Load DICOM header
                            headerData = load(headerPath, 'info');
                            info = headerData.info;
                            
                            % Extract frame index from filename (e.g., 'frame03.mat')
                            frameIndex = 1;
                            frameMatch = regexp(app.Projections(i).name, 'frame(\d+)\.mat', 'tokens');
                            if ~isempty(frameMatch)
                                frameIndex = str2double(frameMatch{1}{1});
                            end
                            
                            % Try to get GantryAngle from ExposureSequence (named fields like Item_3)
                            gantryAngle = NaN;
                            if isfield(info, 'ExposureSequence')
                                itemName = sprintf('Item_%d', frameIndex);
                                if isfield(info.ExposureSequence, itemName)
                                    exposure = info.ExposureSequence.(itemName);
                                    if isfield(exposure, 'GantryAngle')
                                        gantryAngle = exposure.GantryAngle;
                                    end
                                end
                            end
                            
                            % Fallback to global GantryAngle if per-frame not found
                            if isnan(gantryAngle) && isfield(info, 'GantryAngle')
                                gantryAngle = info.GantryAngle;
                            end
                            
                            % Final projection angle
                            projectionAngle = gantryAngle + 90 - offset;
                        end
    
                        % Change angles to be between 0 and 360
                        while projectionAngle > 360
                            projectionAngle = projectionAngle - 360;
                        end
                        while projectionAngle < 0
                            projectionAngle = projectionAngle + 360;
                        end
                        
                        % Save the projection angle to a csv file
                        app.Projections(i).angle = projectionAngle;
                        fprintf(file, '%.4f\n', projectionAngle);
                        d.Value = (i/length(app.Projections))/4; 
                        
                    end

                    angles = [app.Projections.angle];
                    angleDelta = abs(mode(diff(angles)));
                else
                    % Prospective mode, get angles from start/stop angle.
                    if app.AllCheckBox.Value
                        % angles = app.StartAngleValue.Value:app.angleDiff:app.StopAngleValue.Value;
                        angleDelta = 1.0;
                    else
                        angleDelta = (app.StopAngleValue.Value - app.StartAngleValue.Value) / (app.NumberProj.Value - 1);
                    end
                    angles = app.StartAngleValue.Value:angleDelta:app.StopAngleValue.Value;
                    for i = 1:length(angles)
                        fprintf(file, '%.4f\n', angles(i));
                    end

                    % Set image size - this is typically obtained from the kV images.
                    % app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [768 1024]);

                end
                
                fclose(file);
                d.Message = 'Loading the CT';
                d.Value = 0.25; 
    
                % Water Attenuation
                waterAtt = 0.013;
                
                % Load Plan and get isocentre
                info_plan = dicominfo(app.paths.plan);
                IsoCentrePosition = info_plan.BeamSequence.Item_1.ControlPointSequence.Item_1.IsocenterPosition;
                            
                % Sort out CT z locations
                zLocs{1} = [];
                updatedctHeaders = {};
                for k = 1:length(app.dcmHeaders)   
                    if ~any(zLocs{1} == app.dcmHeaders{k}.ImagePositionPatient(3))
                        zLocs{1} = [zLocs{1},app.dcmHeaders{k}.ImagePositionPatient(3)];
                        updatedctHeaders{end+1} = app.dcmHeaders{k};   
                    end
                end
                
                app.dcmHeaders = updatedctHeaders;
                
                zOrientation = app.dcmHeaders{1}.ImageOrientationPatient(5);
                
                % Positive z is towards the head if image orientation(5) is positive
                if zOrientation > 0
                    [zLocs_sorted{1},zSortIdx{1}] = sort(zLocs{1},'descend');
                else
                    [zLocs_sorted{1},zSortIdx{1}] = sort(zLocs{1},'ascend');
                end
                
                zIdx{1} = 1:length(zSortIdx{1});
                zIdx{1}(zSortIdx{1}) = zIdx{1};
                
                zIdxForEachFile = zeros(1,length(app.dcmHeaders));
                for k = 1:length(app.dcmHeaders) 
                        zIdxForEachFile(k) = zIdx{1}(k);
                end
                
                %% Read CT volumes and stack them up according to z location
                for k = 1:length(app.dcmHeaders)
                    imgStacks{1}(:,:,zIdxForEachFile(k)) = single(dicomread(app.dcmHeaders{k}.Filename));
                end
                
                %% Convert to IEC geometry and scale intensity value
                imgStacks{1} = permute(imgStacks{1},[2 3 1]);
                
                if strcmp(app.fileType,'.his') || strcmp(app.fileType,'.hnd') || strcmp(app.fileType,'.tiff')
                    imgStacks{1} = imgStacks{1}(end:-1:1,:,:); 
                end
                
                if ~isnan(waterAtt)
                    if min(imgStacks{1}(:)) >= 0
                        imgStacks{1} = imgStacks{1} * waterAtt / 1000;
                    else
                        imgStacks{1} = (imgStacks{1} + 1000) * waterAtt / 1000;
                    end
                end
                
                d.Value = 0.35; 
                d.Message = 'Generating structure volume mask';
                
                %% Write to file
                load('mhaHeaderTemplate.mat','mhaHeaderTemplate');
                mhaHeader = mhaHeaderTemplate;
                mhaHeader.BitDepth = 32;
                mhaHeader.DataType = 'float';
                
                mhaHeaders{1} = mhaHeader;
                mhaHeaders{1}.Dimensions = size(imgStacks{1});
                
                mhaHeaders{1}.PixelDimensions(1) = app.dcmHeaders{1}.PixelSpacing(1);
                mhaHeaders{1}.PixelDimensions(2) = zLocs_sorted{1}(1) - zLocs_sorted{1}(2);
                mhaHeaders{1}.PixelDimensions(3) = app.dcmHeaders{1}.PixelSpacing(2);      
                
                if strcmp(app.fileType,'.his') || strcmp(app.fileType,'.hnd') || strcmp(app.fileType,'.tiff')
                    mhaHeaders{1}.Offset(1) = app.dcmHeaders{1}.ImagePositionPatient(1) + IsoCentrePosition(1); 
                else
                    mhaHeaders{1}.Offset(1) = app.dcmHeaders{1}.ImagePositionPatient(1) - IsoCentrePosition(1); 
                end
                mhaHeaders{1}.Offset(3) = app.dcmHeaders{1}.ImagePositionPatient(2) - IsoCentrePosition(2); 
                mhaHeaders{1}.Offset(2) = -1*max(zLocs_sorted{1}) + IsoCentrePosition(3);      
    
                MhaWriter(app,mhaHeaders{1},imgStacks{1},fullfile(app.paths.temp,'CT.mha'));
    
                %% Read RT structures
                mask = RTStructuretoMHA(app, app.SelectStructure.Value, zLocs_sorted);
                
                mask = permute(mask,[1 3 2]);           
    
                if strcmp(app.fileType,'.his') || strcmp(app.fileType,'.hnd') || strcmp(app.fileType,'.tiff')
                    mask = mask(end:-1:1,:,:);
                end 
                          
                
                MhaWriter(app,mhaHeaders{1},mask,fullfile(app.paths.temp,'ROI.mha'));
    
                %% Forward Projections
                % Check for parallel computing toolbox.
                if isempty(ver("parallel"))
                    warning("Please install 'parallel computing' toolbox for GPU processing.")
                end
                if ~isempty(ver("parallel")) && gpuDeviceCount("available")
                    cuda = 'CUDA';
                    fp = 'CudaRayCast';
                else
                    cuda = [];
                    fp = 'Joseph';
                end



                logfile = fopen(fullfile(app.paths.export,'log.txt'), 'a');
                
                % Generate the simulated geometry file

                script_path = mfilename('fullpath');
                fprintf(logfile, "Script path: %s\n", script_path);
                [project_path, ~, ~] = fileparts(mfilename('fullpath'));
                fprintf(logfile, "Project path: %s\n", project_path);
                dep_path = fullfile(project_path, 'Dependencies');
                fprintf(logfile, "Dep path: %s\n", dep_path);
                fprintf(logfile, "Temp path: %s\n", app.paths.temp);
                if ismac
                    exe_path = fullfile(dep_path, 'geometryMac');
                else
                    exe_path = fullfile(dep_path, 'geometry');
                end
                cmd = [exe_path,cuda,' ',...
                    '-i "',fullfile(app.paths.temp, 'ProjectionAngles.csv"'),' ',...
                    '--sid ', num2str(app.SIDvalue.Value),' ',...
                    '--sdd ', num2str(app.SDDvalue.Value),' ',...
                    '--proj_iso_x ', num2str(app.offsetValue.Value),' ',...
                    '-o "',fullfile(app.paths.temp,'Geometry.xml"')];
                fprintf(logfile, '\n--- Geometry Command ---\n%s\n', cmd);
                disp(cmd);
                [status, cmdout] = system(cmd);
                fprintf(logfile, 'Exit status: %d\nOutput:\n%s\n', status, cmdout);
                
                d.Value = 0.5; 
                d.Message = 'Generating the CT forward projections';
                
                % Generate the forward projections
                matrixSize = str2num(app.MatrixSizeDropDown.Value);
                if ismac
                    exe_path = fullfile(dep_path, 'forwardprojectionsMac');
                else
                    exe_path = fullfile(dep_path, 'forwardprojections');
                end
                cmd = [exe_path,cuda,' ',...
                   '-i "',fullfile(app.paths.temp,'CT.mha"'),' ',...
                   '-o "',fullfile(app.paths.temp,'CT_FP.mha"'),' ',...
                   '-g "',fullfile(app.paths.temp,'Geometry.xml"'),' ',...
                   '--fp ',fp,' ',...
                   '--dimension ',num2str(matrixSize(2)),',',num2str(matrixSize(1)),' ',...
                   '--spacing ', num2str(app.PixelSpacingValue.Value)];
                fprintf(logfile, '\n--- CT Forward Projection Command ---\n%s\n', cmd);
                disp(cmd);
                [status, cmdout] = system(cmd);
                fprintf(logfile, 'Exit status: %d\nOutput:\n%s\n', status, cmdout);
                
                d.Value = 0.7; 
                d.Message = 'Generating the structure forward projections';
                
                cmd = [exe_path,cuda,' ',...
                    '-i "',fullfile(app.paths.temp,'ROI.mha"'),' ',...
                   '-o "',fullfile(app.paths.temp,'ROI_FP.mha"'),' ',...
                   '-g "',fullfile(app.paths.temp,'Geometry.xml"'),' ',...
                   '--fp ',fp,' ',...
                   '--dimension ',num2str(matrixSize(2)),',',num2str(matrixSize(1)),' ',...
                   '--spacing ', num2str(app.PixelSpacingValue.Value)];
                fprintf(logfile, '\n--- ROI Forward Projection Command ---\n%s\n', cmd);
                disp(cmd);
                [status, cmdout] = system(cmd);
                fprintf(logfile, 'Exit status: %d\nOutput:\n%s\n', status, cmdout);
                    
                d.Value = 0.9; 
                d.Message = 'Initialising...';
                fclose(logfile);
                
                %% Load the images
                app.currentFrame = 1;
                if app.RedMenu.Checked
                    app.colour = cat(3, ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)));
                elseif app.OrangeMenu.Checked
                    app.colour = cat(3, ones(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2))*0.5, zeros(matrixSize(1),matrixSize(2)));
                elseif app.YellowMenu.Checked
                    app.colour = cat(3, ones(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)));
                elseif app.GreenMenu.Checked
                    app.colour = cat(3, zeros(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)));
                elseif app.BlueMenu.Checked
                    app.colour = cat(3, zeros(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2)));
                elseif app.PurpleMenu.Checked
                    app.colour = cat(3, ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2)));
                end
     
                if app.mode == "alignment" || app.isRetro
                    [app.Projections.confidence] = deal(0);
                end
                
                [~,app.DRRs] = MhaReader(app, fullfile(app.paths.temp,'CT_FP.mha'));
                [~,app.Masks] = MhaReader(app, fullfile(app.paths.temp,'ROI_FP.mha'));
                app.originalMasks = app.Masks;

                if app.mode == "selection"
                    % Crop the DRRs/Masks if collimator is selected.
                    matrixSize = str2num(app.MatrixSizeDropDown.Value);
                    xMatrixSize = matrixSize(2);
                    yMatrixSize = matrixSize(1);
                    pixelSpacing = app.PixelSpacingValue.Value;
                    ccSizeMM = str2num(app.CollimatorCassetteDropDown.Value);
                    if ~isempty(ccSizeMM)
                        ccSize = floor(ccSizeMM / pixelSpacing);
                        xCcSize = ccSize(1);
                        yCcSize = ccSize(2);
                        if xCcSize > xMatrixSize
                            xCcSize = xMatrixSize;
                        end
                        if yCcSize > yMatrixSize
                            yCcSize = yMatrixSize;
                        end
                        % Collimator cassettes are (W, L) with W=x, L=y.
                        % Remove x/yCrop from both sides.
                        xCrop = floor((xMatrixSize - xCcSize) / 2);
                        yCrop = floor((yMatrixSize - yCcSize) / 2);
                        if xCrop < 0
                            xCrop = 0;
                        end
                        if yCrop < 0
                            yCrop = 0;
                        end
                        % app.DRRs = app.DRRs(xCrop + 1:xCrop + xCcSize, yCrop + 1:yCrop + yCcSize, :);
                        % app.Masks = app.Masks(xCrop + 1:xCrop + xCcSize, yCrop + 1:yCrop + yCcSize, :);

                        % Darken the cropped voxels, don't remove.
                        cropMask = false(xMatrixSize, yMatrixSize);
                        cropMask(xCrop + 1:xCrop + xCcSize, yCrop + 1:yCrop + yCcSize) = true;
                        
                        % Apply mask to all slices
                        app.DRRs = app.DRRs .* cropMask;
                        app.Masks = app.Masks .* cropMask;
                    else
                        xCrop = 0;
                        yCrop = 0;
                    end


                    % Add 10mm margin - plus collimated crop.
                    app.marginX = floor(app.margin / app.PixelSpacingValue.Value) + xCrop;
                    app.marginY = floor(app.margin / app.PixelSpacingValue.Value) + yCrop;

                    % Check how many masks are missing or cropped (with a 10mm crop applied to the image).
                    nMasks = size(app.Masks, 3);
                    maskPassed = true(nMasks, 1);
                    for i = 1:nMasks
                        mask = app.Masks(:, :, i);
                        if all(mask == 0, 'all')  % Empty mask.
                            maskPassed(i) = false;
                            continue;
                        else
                            % Crop mask by margin.
                            mask = mask(app.marginX:end - app.marginX + 1, app.marginY:end - app.marginY + 1);
        
                            % Check for foreground edge voxels.
                            maskHasBorderPixels = ...
                                any(mask(1, :) ~= 0)   || ...
                                any(mask(end, :) ~= 0) || ...
                                any(mask(:, 1) ~= 0)   || ...
                                any(mask(:, end) ~= 0);      
    
                            if all(mask == 0, 'all') || maskHasBorderPixels
                                maskPassed(i) = false;
                                continue;
                            end
                        end
                    end
    
                    % Indicate whether this patient passes or not.
                    nPassed = sum(maskPassed);
                    angleCovered = nPassed * angleDelta;
                    if angleCovered >= app.minAngle
                        uialert(app.ContourAlignmentToolUIFigure, [['Passed the selection criteria.'], "", ['Contours were present for more than 180°. Were present for ' ...
                        num2str(angleCovered, '%.1f'), '° of the ', num2str(nMasks * angleDelta, "%.1f"), ...
                        '° treatment arc, or ', num2str(nPassed), '/', num2str(nMasks), ' images.']], 'Passed', 'Icon', 'success');
                    else
                        uialert(app.ContourAlignmentToolUIFigure, [['Failed the selection criteria.'], "", ['Contours were not present for more than 180°. Were only present for ' ...
                        num2str(angleCovered, '%.1f'), '° of the ', num2str(nMasks * angleDelta, "%.1f"), ...
                        '° treatment arc, or ', num2str(nPassed), '/', num2str(nMasks), ' images.']], 'Failed', 'Icon', 'error');
                    end

                    
                end

                close(d)
                
                if app.mode == "alignment"
                    % Adds projections to the list of unlabelled images.
                    for k1 = 1:length(app.Projections)
                        uitreenode(app.C0Node,"Text", sprintf('Image %d (%.2f%c)',k1,app.Projections(k1).angle,char(176))); 
                    end
                else
                    % Assign DRRs to passed/failed categories.
                    n_DRRs = size(app.DRRs, 3);
                    for i = 1:n_DRRs
                        if maskPassed(i)
                            uitreenode(app.PassedNode, "Text", sprintf('Image %d (%.2f%c)', i, angles(i), char(176)));
                        else
                            uitreenode(app.FailedNode, "Text", sprintf('Image %d (%.2f%c)', i, angles(i), char(176)));
                        end
                    end
                end
                            
                % Update the image details dialogue
                updateImageDetails(app)
                loadImages(app)               
                updatePlot(app) 
                expand(app.Tree)
                
                % Enable the contour alignment components
                app.DataProcessingPanel.Visible = 'off';
                app.NavigationPanel.Enable = 'on';
                if app.mode == "alignment"
                    app.ContourAlignmentPanel.Enable = 'on';
                    app.ExportMenu.Enable = 'on';
                    app.ExportAsMenu.Enable = 'on';
                    app.QuickExportMenu.Visible = 'on';
                end
                if app.mode == "alignment" || app.isRetro
                    app.DRRViewerMenu.Enable = 'on';
                end
                app.ContrastAdjustmentPanel.Enable = 'on';
                app.UIAxes.Visible = 'on';
                app.ContourColourMenu.Enable = 'on';
                app.ContourFillMenu.Enable = 'on';
                app.NewFractionMenu.Enable = 'on';
                app.NewPatientMenu.Enable = 'on';
                app.InvertIntensityMenu.Enable = 'on';
            
            % If there is an error during data processing, re-enable the
            % browsing buttons and save the error log.
            catch ME
                app.ProjectionsBrowse.Enable = 'on';
                app.StructureBrowse.Enable = 'on';
                app.CTBrowse.Enable = 'on';
                app.PlanBrowse.Enable = 'on';
                app.MasterBrowse.Enable = 'on';
                app.kVImagingParametersPanel.Enable = 'on';
                app.PatientDropDown.Enable = 'on';
                app.FractionDropDown.Enable = 'on';
                app.SelectStructure.Enable = 'on';
                app.StartProcessing.Enable = 'on';
                
                filepath = fullfile(app.paths.export, 'error_log.txt');
                file = fopen(filepath, 'w');
                fprintf(file, getReport(ME,'extended','hyperlinks','off'));
                fclose(file);

                message = sprintf('An error during data processing. Error log file located in export folder: %s',filepath);
                uialert(app.ContourAlignmentToolUIFigure,message,'Data Processing Error');
            end
        end

        % Button pushed function: First
        function FirstButtonPushed(app, event)
            % Go to the first frame
            save(app)
            app.currentFrame = 1;  
            loadImages(app)
            updatePlot(app)
            updateImageDetails(app)
        end

        % Button pushed function: Previous
        function PreviousPushed(app, event)
            % Go to the previous frame if possible        
            if app.currentFrame - 1 > 0
                save(app)
                app.currentFrame = app.currentFrame - 1;
                loadImages(app)
                updatePlot(app)
                updateImageDetails(app)
            else
                save(app)
                message = sprintf('Start of images');
                uialert(app.ContourAlignmentToolUIFigure,message,'Data Processing','Icon','warning');
            end 
        end

        % Button pushed function: Next
        function NextPushed(app, event)
            if app.mode == "alignment" || app.isRetro
                n_images = length(app.Projections);
            else
                n_images = size(app.DRRs, 3);
            end
            
            % Go to the next frame if possible
            if app.currentFrame + 1 <= n_images
                save(app)
                app.currentFrame = app.currentFrame + 1;
                loadImages(app)
                updatePlot(app)
                updateImageDetails(app)

            else
                save(app)
                message = sprintf('End of images');
                uialert(app.ContourAlignmentToolUIFigure,message,'Data Processing','Icon','warning');
            
            end
        end

        % Button pushed function: End
        function EndButtonPushed(app, event)
            if app.mode == "alignment" || app.isRetro
                n_images = length(app.Projections);
            else
                n_images = size(app.DRRs, 3);
            end

            % Go to the last frame
            save(app)
            app.currentFrame = n_images;
            loadImages(app)
            updatePlot(app)
            updateImageDetails(app)
        end

        % Selection changed function: Tree
        function TreeSelectionChanged(app, event)
            % Go to the selected frame 
            if strcmp(app.Tree.SelectedNodes.Text(1:5),'Image')
                save(app)
                k = strfind(app.Tree.SelectedNodes.Text,' ');
                app.currentFrame = str2double(app.Tree.SelectedNodes.Text(k(1)+1:k(2)-1));
                loadImages(app)
                updatePlot(app)
                updateImageDetails(app)
            end
        end

        % Image clicked function: N
        function N_ButtonPushed(app, event)
            % Shift the contour in the direction of the button pressed
            app.MaskOutline(1,:) = [];
            app.MaskOutline(end+1,:) = 0;
            app.Mask(1,:) = [];
            app.Mask(end+1,:) = 0;
            
            updatePlot(app)
        end

        % Image clicked function: E
        function E_ButtonPushed(app, event)
            % Shift the contour in the direction of the button pressed
            blank = zeros(size(app.MaskOutline));
            app.MaskOutline(:,end) = [];
            blank(:,2:end) = app.MaskOutline;
            app.MaskOutline = blank;

            blank = zeros(size(app.Mask));
            app.Mask(:,end) = [];
            blank(:,2:end) = app.Mask;
            app.Mask = blank;
                        
            updatePlot(app)
        end

        % Image clicked function: S
        function S_ButtonPushed(app, event)
            % Shift the contour in the direction of the button pressed
            blank = zeros(size(app.MaskOutline));
            app.MaskOutline(end,:) = [];
            blank(2:end,:) = app.MaskOutline;
            app.MaskOutline = blank;

            blank = zeros(size(app.Mask));
            app.Mask(end,:) = [];
            blank(2:end,:) = app.Mask;
            app.Mask = blank;
            
            updatePlot(app)
        end

        % Image clicked function: W
        function W_ButtonPushed(app, event)
            % Shift the contour in the direction of the button pressed
            app.MaskOutline(:,1) = [];
            app.MaskOutline(:,end+1) = 0;
            app.Mask(:,1) = [];
            app.Mask(:,end+1) = 0;
            
            updatePlot(app)
        end

        % Image clicked function: NE
        function NE_ButtonPushed(app, event)
            % Shift the contour in the direction of the button pressed
            N_ButtonPushed(app, event)
            E_ButtonPushed(app, event)
        end

        % Image clicked function: SE
        function SE_ButtonPushed(app, event)
            % Shift the contour in the direction of the button pressed
            S_ButtonPushed(app, event)
            E_ButtonPushed(app, event)
        end

        % Image clicked function: SW
        function SW_ButtonPushed(app, event)
            % Shift the contour in the direction of the button pressed
            S_ButtonPushed(app, event)
            W_ButtonPushed(app, event)
        end

        % Image clicked function: NW
        function NW_ButtonPushed(app, event)
            % Shift the contour in the direction of the button pressed    
            N_ButtonPushed(app, event)
            W_ButtonPushed(app, event)
        end

        % Image clicked function: reset
        function ResetContourButtonPushed(app, event)
            % Reset the contour position to the original location
            if strcmp(app.fileType,'.his') 
                app.Mask = imrotate(app.Masks(:,:,app.currentFrame),-90);
            else
                app.Mask = permute(app.originalMasks(:,:,app.currentFrame),[2 1 3]);
            end     
            
            app.Mask = imbinarize(app.Mask,0);
            app.MaskOutline = boundarymask(app.Mask);
            app.C0Button.Value = 1;
            
            updatePlot(app)  
        end

        % Value changed function: LowerSlider, UpperSlider
        function SliderValueChanged(app, event)
            % Update the contrast if the slider is changed   
            if app.contrastAutoButton.Value
                app.contrastManualButton.Value = 1;
                app.contrastAutoButton.Value = 0;
            end

            updatePlot(app)  
        end

        % Value changed function: contrastAutoButton, 
        % ...and 3 other components
        function ContrastAdjustment(app, event)
            app.contrastManualButton.Value = 0;
            app.contrastAutoButton.Value = 0;
            app.contrastContourButton.Value = 0;
            app.contrastROIButton.Value = 0;

            % Working on DRR image if looking at prospective patients.
            if app.mode == "alignment" || app.isRetro
                image = app.projection;
            else
                image = app.DRR;
            end
            
            switch event.Source.Text
                
                case 'Manual'
                    app.contrastManualButton.Value = 1;
                    app.LowerSlider.Value = 0;
                    app.UpperSlider.Value = 1;
                    
                case 'Auto'
                    app.contrastAutoButton.Value = 1;
                    
                case 'Contour'
                    app.contrastContourButton.Value = 1;
                    app.LowerSlider.Value = 0;
                    app.UpperSlider.Value = 1;
                    
                case 'ROI'
                    app.contrastROIButton.Value = 1;
            
                    if app.ClickdragCheckBox.Value
                        app.pointerManager.traverseFcn = [];
                    end
                    app.ROIselectionLabel.Visible = 'on';
                    
                    ROI = drawrectangle(app.UIAxes);
                    matrixSize = str2num(app.MatrixSizeDropDown.Value);
                    ROI.Position(1) = round(ROI.Position(1)/app.displaySize(1)*matrixSize(2));
                    ROI.Position(3) = round(ROI.Position(3)/app.displaySize(1)*matrixSize(2));
                    ROI.Position(2) = round(ROI.Position(2)/app.displaySize(2)*matrixSize(1));
                    ROI.Position(4) = round(ROI.Position(4)/app.displaySize(2)*matrixSize(1));
                     
                    
                    app.LowerSlider.Value = 0;
                    app.UpperSlider.Value = 1;
                    
                    
                    app.ROIrange(1) = min(min(image(ROI.Position(2):ROI.Position(2)+ROI.Position(4),...
                        ROI.Position(1):ROI.Position(1)+ROI.Position(3))));
                    app.ROIrange(2) = max(max(image(ROI.Position(2):ROI.Position(2)+ROI.Position(4),...
                        ROI.Position(1):ROI.Position(1)+ROI.Position(3))));
                    
                    delete(ROI)
                    
                    app.ROIselectionLabel.Visible = 'off';
                    
                    if app.ClickdragCheckBox.Value
                        app.pointerManager.traverseFcn = @app.moveContour;
                    end
            end
            
            updatePlot(app)  
        end

        % Menu selected function: BlueMenu, GreenMenu, OrangeMenu, 
        % ...and 3 other components
        function ContourColourSelection(app, event)
            % Change the contour colour
            matrixSize = str2num(app.MatrixSizeDropDown.Value);
            switch event.Source.Text
                case 'Red'
                    app.RedMenu.Checked = 1;
                    [app.OrangeMenu.Checked,app.YellowMenu.Checked,app.GreenMenu.Checked,app.BlueMenu.Checked,app.PurpleMenu.Checked] = deal(0);
                    app.colour = cat(3, ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)));
                    
                case 'Orange'
                    app.OrangeMenu.Checked = 1;
                    [app.RedMenu.Checked,app.YellowMenu.Checked,app.GreenMenu.Checked,app.BlueMenu.Checked,app.PurpleMenu.Checked] = deal(0);
                    app.colour = cat(3, ones(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2))*0.5, zeros(matrixSize(1),matrixSize(2)));
                    
                case 'Yellow'
                    app.YellowMenu.Checked = 1;
                    [app.RedMenu.Checked,app.OrangeMenu.Checked,app.GreenMenu.Checked,app.BlueMenu.Checked,app.PurpleMenu.Checked] = deal(0);
                    app.colour = cat(3, ones(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)));
                    
                case 'Green'
                    app.GreenMenu.Checked = 1;
                    [app.RedMenu.Checked,app.OrangeMenu.Checked,app.YellowMenu.Checked,app.BlueMenu.Checked,app.PurpleMenu.Checked] = deal(0);
                    app.colour = cat(3, zeros(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)));
                    
                case 'Blue'
                    app.BlueMenu.Checked = 1;
                    [app.RedMenu.Checked,app.OrangeMenu.Checked,app.YellowMenu.Checked,app.GreenMenu.Checked,app.PurpleMenu.Checked] = deal(0);
                    app.colour = cat(3, zeros(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2)));
                    
                case 'Purple'
                    app.PurpleMenu.Checked = 1;
                    [app.RedMenu.Checked,app.OrangeMenu.Checked,app.YellowMenu.Checked,app.GreenMenu.Checked,app.BlueMenu.Checked] = deal(0);
                    app.colour = cat(3, ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)), ones(matrixSize(1),matrixSize(2)));
            end
  
            updatePlot(app)
        end

        % Window key press function: ContourAlignmentToolUIFigure
        function ContourAlignmentToolUIFigureKeyPress(app, event)
            if strcmp(app.NavigationPanel.Enable,'off')
                return
            end
            
            % Shift the contour using the arrow keys
            key = event.Key;
                        
            switch key
                case 'rightarrow'
                    E_ButtonPushed(app, event)
                case 'leftarrow'
                    W_ButtonPushed(app, event)
                case 'uparrow'
                    N_ButtonPushed(app, event)
                case 'downarrow'
                    S_ButtonPushed(app, event)
                case {'0', 'backquote','numpad0'} 
                    app.C0Button.Value = 1;
                case {'1','numpad1'} 
                    app.C1Button.Value = 1;
                case {'2','numpad2'} 
                    app.C2Button.Value = 1;
                case {'3','numpad3'} 
                    app.C3Button.Value = 1;
                case {'4','numpad4'} 
                    app.C4Button.Value = 1;
                case {'5','numpad5'} 
                    app.C5Button.Value = 1;
                case 'comma'
                    PreviousPushed(app, event)
                case 'period'
                    NextPushed(app, event)
                case 'space'
                    if app.dispContour
                        app.dispContour = 0;
                    else
                        app.dispContour = 1;
                    end
                    updatePlot(app)
            end
        end

        % Window button down function: ContourAlignmentToolUIFigure
        function ContourAlignmentToolUIFigureWindowButtonDown(app, event)
            % Click and drag the contour if the contour is clicked on
            app.ContourAlignmentToolUIFigure.Pointer = 'arrow'; % Resolves bug with drawrectangle cross hairs
            iptSetPointerBehavior(app.UIAxes, app.pointerManager)
            iptPointerManager(app.ContourAlignmentToolUIFigure,'enable');
            set(app.ContourAlignmentToolUIFigure,'WindowButtonMotionFcn',@(~,~)NaN)
        end

        % Window button up function: ContourAlignmentToolUIFigure
        function ContourAlignmentToolUIFigureWindowButtonUp(app, event)
            % Drop the contour when the button is released
            iptPointerManager(app.ContourAlignmentToolUIFigure,'disable');
        end

        % Callback function: ContourAlignmentToolUIFigure, ExitMenu
        function ContourAlignmentToolUIFigureCloseRequest(app, event)
            % Create a dialogue box for the app close request
            uiconfirm(app.ContourAlignmentToolUIFigure,'Close program?','Contour Alignment Tool',...
                'CloseFcn',@(src,event)closeapp(app,src,event));            
        end

        % Value changed function: ClickdragCheckBox
        function ClicknDrag(app, event)
            % Enable/disable the click and drag feature
            if app.ClickdragCheckBox.Value
                app.pointerManager.traverseFcn = @app.moveContour;
                app.clickdragstatus.Text = 'ON';
                app.clickdragstatus.FontColor = [0.31,0.80,0.00];
                elsea
                app.pointerManager.traverseFcn = [];
                app.clickdragstatus.Text = 'OFF';
                app.clickdragstatus.FontColor = [0.94,0.02,0.02];
            end
            
        end

        % Image clicked function: DirectoriesImage
        function DirectoriesImageClicked(app, event)
            
        end

        % Size changed function: DataProcessingPanel
        function DataProcessingPanelSizeChanged(app, event)
            position = app.DataProcessingPanel.Position;
            
        end

        % Callback function
        function FractionDropDownOpening(app, event)
            
        end

        % Callback function
        function patientSelected(app, event)
            
        end

        % Callback function
        function onPatientDropDownChange(app, event)
           
        end

        % Value changed function: FractionDropDown
        function OnFractionDropDownChanged(app, event)
            % Determine if prospective or retrospective selection.
            if app.mode == "selection"
                if app.FractionDropDown.Value == "Select fraction"
                    disp('setting pros')
                    app.isRetro = false;

                    % Show start/stop angles.
                    app.StartAngleLabel.Visible = 'on';
                    app.StartAngleLamp.Visible = 'on';
                    app.StartAngleValue.Visible = 'on';
                    app.StopAngleLabel.Visible = 'on';
                    app.StopAngleLamp.Visible = 'on';
                    app.StopAngleValue.Visible = 'on';
                else
                    app.isRetro = true;

                    % Hide start/stop angles.
                    app.StartAngleLabel.Visible = 'off';
                    app.StartAngleLamp.Visible = 'off';
                    app.StartAngleValue.Visible = 'off';
                    app.StopAngleLabel.Visible = 'off';
                    app.StopAngleLamp.Visible = 'off';
                    app.StopAngleValue.Visible = 'off';
                end
            end
            
            updateProjectionsDir(app,0)
        end

        % Value changed function: PatientDropDown
        function OnPatientDropDownChanged(app, event)
            if app.mode == "selection"
                app.paths.export = fullfile(app.paths.master, app.folders.images, app.PatientDropDown.Value);
                app.StartAngleLabel.Visible = 'on';
                app.StartAngleLamp.Visible = 'on';
                app.StartAngleValue.Visible = 'on';
                app.StopAngleLabel.Visible = 'on';
                app.StopAngleLamp.Visible = 'on';
                app.StopAngleValue.Visible = 'on';
                [app.StartAngleLamp.Color, app.StopAngleLamp.Color] = deal([0.90,0.90,0.90]);
            end
            
            % If the user selects a new patient from the dropdown, load the
            % new list of fraction folders.
            if ~strcmp(app.PatientDropDown.Value,'Select patient')
            
                fractions = dir(fullfile(app.paths.master, app.folders.images, app.PatientDropDown.Value));
                fractions(strcmp({fractions.name}, "temp")) = [];
                dirFlags = [fractions.isdir];
                dirFlags(1:2) = 0;
                fractions = fractions(dirFlags);
                fractions = struct2cell(fractions);
                app.FractionDropDown.Items = [{'Select fraction'},fractions(1,:)];  
                app.FractionDropDown.Value = 'Select fraction'; 
                
                updateDICOMDir(app,0)
                
                app.FractionDropDown.Enable = 'on';
                
            else
                app.CTLabel.Text = 'CT folder not selected';
                app.CTLabel.FontColor = [1.00,0.41,0.16];
                
                app.PlanLabel.Text = 'Plan file not selected';
                app.PlanLabel.FontColor = [1.00,0.41,0.16];
                
                app.StructureLabel.Text = 'Structure set file not selected';
                app.StructureLabel.FontColor = [1.00,0.41,0.16];
                app.SelectStructure.Visible = 'off';
                
                app.FractionDropDown.Items = {'Fraction'};
                app.FractionDropDown.Enable = 'off';
                
            end
            
            app.ProjectionsLabel.Text = 'Intrafraction images folder not selected';
            app.ProjectionsLabel.FontColor = [1.00,0.41,0.16];
            [app.PixelSpacingLamp.Color,app.SIDLamp.Color,app.SDDLamp.Color,app.offsetLamp.Color] = deal([0.90,0.90,0.90]);
            app.parametersWarning.Visible = 'off';
        end

        % Callback function
        function OnFractionDropDownOpened(app, event)
            
        end

        % Value changed function: MachineDropDown
        function OnMachineDropDownChanged(app, event)
            value = app.MachineDropDown.Value;

            if value == "Elekta"
                app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [512 512]);
                app.PixelSpacingValue.Value = 0.8;
                app.CollimatorCassetteDropDown.Visible = 'on';
                app.CollimatorCassetteDropDownLabel.Visible = 'on';
            elseif value == "Varian"
                app.MatrixSizeDropDown.Value = sprintf('[%d %d]', [768 1024]); 
                app.PixelSpacingValue.Value = 0.388;
                app.CollimatorCassetteDropDown.Visible = 'off';
                app.CollimatorCassetteDropDownLabel.Visible = 'off';
            end
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Get the file path for locating images
            pathToMLAPP = fileparts(mfilename('fullpath'));

            % Create ContourAlignmentToolUIFigure and hide until all components are created
            app.ContourAlignmentToolUIFigure = uifigure('Visible', 'off');
            app.ContourAlignmentToolUIFigure.Color = [0.651 0.651 0.651];
            app.ContourAlignmentToolUIFigure.Position = [498 298 1134 602];
            app.ContourAlignmentToolUIFigure.Name = 'Contour Alignment Tool';
            app.ContourAlignmentToolUIFigure.Icon = 'icon_48.png';
            app.ContourAlignmentToolUIFigure.CloseRequestFcn = createCallbackFcn(app, @ContourAlignmentToolUIFigureCloseRequest, true);
            app.ContourAlignmentToolUIFigure.WindowButtonDownFcn = createCallbackFcn(app, @ContourAlignmentToolUIFigureWindowButtonDown, true);
            app.ContourAlignmentToolUIFigure.WindowButtonUpFcn = createCallbackFcn(app, @ContourAlignmentToolUIFigureWindowButtonUp, true);
            app.ContourAlignmentToolUIFigure.WindowKeyPressFcn = createCallbackFcn(app, @ContourAlignmentToolUIFigureKeyPress, true);

            % Create FileMenu
            app.FileMenu = uimenu(app.ContourAlignmentToolUIFigure);
            app.FileMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.FileMenu.Text = '  File  ';

            % Create ExportMenu
            app.ExportMenu = uimenu(app.FileMenu);
            app.ExportMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.ExportMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ExportMenu.Enable = 'off';
            app.ExportMenu.Accelerator = 'e';
            app.ExportMenu.Text = 'Export';

            % Create ExportAsMenu
            app.ExportAsMenu = uimenu(app.FileMenu);
            app.ExportAsMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.ExportAsMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ExportAsMenu.Enable = 'off';
            app.ExportAsMenu.Text = 'Export As...';

            % Create NewPatientMenu
            app.NewPatientMenu = uimenu(app.FileMenu);
            app.NewPatientMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.NewPatientMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.NewPatientMenu.Enable = 'off';
            app.NewPatientMenu.Separator = 'on';
            app.NewPatientMenu.Accelerator = 'p';
            app.NewPatientMenu.Text = 'New Patient...';

            % Create NewFractionMenu
            app.NewFractionMenu = uimenu(app.FileMenu);
            app.NewFractionMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.NewFractionMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.NewFractionMenu.Enable = 'off';
            app.NewFractionMenu.Accelerator = 'f';
            app.NewFractionMenu.Text = 'New Fraction...';

            % Create ExitMenu
            app.ExitMenu = uimenu(app.FileMenu);
            app.ExitMenu.MenuSelectedFcn = createCallbackFcn(app, @ContourAlignmentToolUIFigureCloseRequest, true);
            app.ExitMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ExitMenu.Separator = 'on';
            app.ExitMenu.Accelerator = 'q';
            app.ExitMenu.Text = 'Exit';

            % Create DisplayMenu
            app.DisplayMenu = uimenu(app.ContourAlignmentToolUIFigure);
            app.DisplayMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.DisplayMenu.Text = '  Display  ';

            % Create DRRViewerMenu
            app.DRRViewerMenu = uimenu(app.DisplayMenu);
            app.DRRViewerMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.DRRViewerMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.DRRViewerMenu.Enable = 'off';
            app.DRRViewerMenu.Accelerator = 'd';
            app.DRRViewerMenu.Text = 'DRR Viewer';

            % Create ContourFillMenu
            app.ContourFillMenu = uimenu(app.DisplayMenu);
            app.ContourFillMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.ContourFillMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ContourFillMenu.Enable = 'off';
            app.ContourFillMenu.Separator = 'on';
            app.ContourFillMenu.Text = 'Contour Fill';

            % Create ContourColourMenu
            app.ContourColourMenu = uimenu(app.DisplayMenu);
            app.ContourColourMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ContourColourMenu.Enable = 'off';
            app.ContourColourMenu.Text = 'Contour Colour';

            % Create RedMenu
            app.RedMenu = uimenu(app.ContourColourMenu);
            app.RedMenu.MenuSelectedFcn = createCallbackFcn(app, @ContourColourSelection, true);
            app.RedMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.RedMenu.Checked = 'on';
            app.RedMenu.Text = 'Red';

            % Create OrangeMenu
            app.OrangeMenu = uimenu(app.ContourColourMenu);
            app.OrangeMenu.MenuSelectedFcn = createCallbackFcn(app, @ContourColourSelection, true);
            app.OrangeMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.OrangeMenu.Text = 'Orange';

            % Create YellowMenu
            app.YellowMenu = uimenu(app.ContourColourMenu);
            app.YellowMenu.MenuSelectedFcn = createCallbackFcn(app, @ContourColourSelection, true);
            app.YellowMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.YellowMenu.Text = 'Yellow';

            % Create GreenMenu
            app.GreenMenu = uimenu(app.ContourColourMenu);
            app.GreenMenu.MenuSelectedFcn = createCallbackFcn(app, @ContourColourSelection, true);
            app.GreenMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.GreenMenu.Text = 'Green';

            % Create BlueMenu
            app.BlueMenu = uimenu(app.ContourColourMenu);
            app.BlueMenu.MenuSelectedFcn = createCallbackFcn(app, @ContourColourSelection, true);
            app.BlueMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.BlueMenu.Text = 'Blue';

            % Create PurpleMenu
            app.PurpleMenu = uimenu(app.ContourColourMenu);
            app.PurpleMenu.MenuSelectedFcn = createCallbackFcn(app, @ContourColourSelection, true);
            app.PurpleMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.PurpleMenu.Text = 'Purple';

            % Create InvertIntensityMenu
            app.InvertIntensityMenu = uimenu(app.DisplayMenu);
            app.InvertIntensityMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.InvertIntensityMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.InvertIntensityMenu.Enable = 'off';
            app.InvertIntensityMenu.Separator = 'on';
            app.InvertIntensityMenu.Checked = 'on';
            app.InvertIntensityMenu.Text = 'Invert Intensity';

            % Create QuickExportMenu
            app.QuickExportMenu = uimenu(app.ContourAlignmentToolUIFigure);
            app.QuickExportMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.QuickExportMenu.Visible = 'off';
            app.QuickExportMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.QuickExportMenu.Text = '  Quick Export  ';

            % Create HelpMenu
            app.HelpMenu = uimenu(app.ContourAlignmentToolUIFigure);
            app.HelpMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.HelpMenu.Text = '  Help  ';

            % Create ManualMenu
            app.ManualMenu = uimenu(app.HelpMenu);
            app.ManualMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.ManualMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ManualMenu.Text = 'Manual';

            % Create KeyboardShortcutsMenu
            app.KeyboardShortcutsMenu = uimenu(app.HelpMenu);
            app.KeyboardShortcutsMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.KeyboardShortcutsMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.KeyboardShortcutsMenu.Text = 'Keyboard Shortcuts';

            % Create CheckforUpdatesMenu
            app.CheckforUpdatesMenu = uimenu(app.HelpMenu);
            app.CheckforUpdatesMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.CheckforUpdatesMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.CheckforUpdatesMenu.Separator = 'on';
            app.CheckforUpdatesMenu.Text = 'Check for Updates';

            % Create ReportMenu
            app.ReportMenu = uimenu(app.HelpMenu);
            app.ReportMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.ReportMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ReportMenu.Accelerator = 'i';
            app.ReportMenu.Text = 'Report an Issue...';

            % Create AboutMenu
            app.AboutMenu = uimenu(app.HelpMenu);
            app.AboutMenu.MenuSelectedFcn = createCallbackFcn(app, @MenuSelections, true);
            app.AboutMenu.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.AboutMenu.Separator = 'on';
            app.AboutMenu.Text = 'About';

            % Create GridLayout
            app.GridLayout = uigridlayout(app.ContourAlignmentToolUIFigure);
            app.GridLayout.ColumnWidth = {205, 379, '5.28x', 320};
            app.GridLayout.RowHeight = {'5.28x', 186, 111, 35};
            app.GridLayout.ColumnSpacing = 4;
            app.GridLayout.RowSpacing = 4;
            app.GridLayout.Padding = [0 0 0 0];
            app.GridLayout.BackgroundColor = [0.651 0.651 0.651];

            % Create UIAxes
            app.UIAxes = uiaxes(app.GridLayout);
            app.UIAxes.Toolbar.Visible = 'off';
            app.UIAxes.Layout.Row = [1 4];
            app.UIAxes.Layout.Column = [1 3];
            app.UIAxes.Visible = 'off';

            % Create ROIselectionLabel
            app.ROIselectionLabel = uilabel(app.GridLayout);
            app.ROIselectionLabel.HorizontalAlignment = 'center';
            app.ROIselectionLabel.FontColor = [0.851 0.3255 0.098];
            app.ROIselectionLabel.Visible = 'off';
            app.ROIselectionLabel.Layout.Row = 4;
            app.ROIselectionLabel.Layout.Column = [1 3];
            app.ROIselectionLabel.Text = 'Click and drag the mouse to draw the ROI for auto-contrast adjustment ';

            % Create NavigationPanel
            app.NavigationPanel = uipanel(app.GridLayout);
            app.NavigationPanel.Enable = 'off';
            app.NavigationPanel.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.NavigationPanel.BorderType = 'none';
            app.NavigationPanel.Title = 'Navigation';
            app.NavigationPanel.BackgroundColor = [0.902 0.902 0.902];
            app.NavigationPanel.Layout.Row = 1;
            app.NavigationPanel.Layout.Column = 4;
            app.NavigationPanel.FontWeight = 'bold';
            app.NavigationPanel.FontSize = 12.5;

            % Create Tree
            app.Tree = uitree(app.NavigationPanel);
            app.Tree.SelectionChangedFcn = createCallbackFcn(app, @TreeSelectionChanged, true);
            app.Tree.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.Tree.Position = [7 6 309 195];

            % Create C0Node
            app.C0Node = uitreenode(app.Tree);
            app.C0Node.Text = '0: Unlabelled';

            % Create C1Node
            app.C1Node = uitreenode(app.Tree);
            app.C1Node.Text = '1: Not at all confident';

            % Create C2Node
            app.C2Node = uitreenode(app.Tree);
            app.C2Node.Text = '2: Not very confident';

            % Create C3Node
            app.C3Node = uitreenode(app.Tree);
            app.C3Node.Text = '3: Neither';

            % Create C4Node
            app.C4Node = uitreenode(app.Tree);
            app.C4Node.Text = '4: Fairly confident';

            % Create C5Node
            app.C5Node = uitreenode(app.Tree);
            app.C5Node.Text = '5: Very confident';

            % Create PassedNode
            app.PassedNode = uitreenode(app.Tree);
            app.PassedNode.Text = 'Passed';

            % Create FailedNode
            app.FailedNode = uitreenode(app.Tree);
            app.FailedNode.Text = 'Failed';

            % Create End
            app.End = uibutton(app.NavigationPanel, 'push');
            app.End.ButtonPushedFcn = createCallbackFcn(app, @EndButtonPushed, true);
            app.End.Icon = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'last.png');
            app.End.IconAlignment = 'center';
            app.End.WordWrap = 'on';
            app.End.BackgroundColor = [0.9608 0.9608 0.9608];
            app.End.FontName = 'Script MT Bold';
            app.End.FontSize = 18;
            app.End.FontWeight = 'bold';
            app.End.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.End.Tooltip = {'Go to last image'};
            app.End.Position = [223 207 25 25];
            app.End.Text = '';

            % Create Next
            app.Next = uibutton(app.NavigationPanel, 'push');
            app.Next.ButtonPushedFcn = createCallbackFcn(app, @NextPushed, true);
            app.Next.Icon = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'next.png');
            app.Next.IconAlignment = 'center';
            app.Next.WordWrap = 'on';
            app.Next.BackgroundColor = [0.9608 0.9608 0.9608];
            app.Next.FontName = 'Script MT Bold';
            app.Next.FontSize = 18;
            app.Next.FontWeight = 'bold';
            app.Next.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.Next.Tooltip = {'Go to next image'};
            app.Next.Position = [189 207 25 25];
            app.Next.Text = '';

            % Create projectionnumber
            app.projectionnumber = uilabel(app.NavigationPanel);
            app.projectionnumber.HorizontalAlignment = 'center';
            app.projectionnumber.FontSize = 18;
            app.projectionnumber.FontWeight = 'bold';
            app.projectionnumber.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.projectionnumber.Position = [136 209 51 22];
            app.projectionnumber.Text = '1';

            % Create Previous
            app.Previous = uibutton(app.NavigationPanel, 'push');
            app.Previous.ButtonPushedFcn = createCallbackFcn(app, @PreviousPushed, true);
            app.Previous.Icon = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'previous.png');
            app.Previous.IconAlignment = 'center';
            app.Previous.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.Previous.FontName = 'Script MT Bold';
            app.Previous.FontSize = 18;
            app.Previous.FontWeight = 'bold';
            app.Previous.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.Previous.Tooltip = {'Go to previous image'};
            app.Previous.Position = [110 207 25 25];
            app.Previous.Text = '';

            % Create First
            app.First = uibutton(app.NavigationPanel, 'push');
            app.First.ButtonPushedFcn = createCallbackFcn(app, @FirstButtonPushed, true);
            app.First.Icon = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'first.png');
            app.First.IconAlignment = 'center';
            app.First.WordWrap = 'on';
            app.First.BackgroundColor = [0.9608 0.9608 0.9608];
            app.First.FontName = 'Script MT Bold';
            app.First.FontSize = 18;
            app.First.FontWeight = 'bold';
            app.First.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.First.Tooltip = {'Go to first image'};
            app.First.Position = [76 207 25 25];
            app.First.Text = '';

            % Create ContrastAdjustmentPanel
            app.ContrastAdjustmentPanel = uipanel(app.GridLayout);
            app.ContrastAdjustmentPanel.Enable = 'off';
            app.ContrastAdjustmentPanel.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ContrastAdjustmentPanel.BorderType = 'none';
            app.ContrastAdjustmentPanel.Title = 'Contrast Adjustment';
            app.ContrastAdjustmentPanel.BackgroundColor = [0.902 0.902 0.902];
            app.ContrastAdjustmentPanel.Layout.Row = [3 4];
            app.ContrastAdjustmentPanel.Layout.Column = 4;
            app.ContrastAdjustmentPanel.FontWeight = 'bold';
            app.ContrastAdjustmentPanel.FontSize = 12.5;

            % Create Histogram
            app.Histogram = uiaxes(app.ContrastAdjustmentPanel);
            app.Histogram.Toolbar.Visible = 'off';
            app.Histogram.Visible = 'off';
            app.Histogram.Position = [1 44 320 80];

            % Create LowerSlider
            app.LowerSlider = uislider(app.ContrastAdjustmentPanel);
            app.LowerSlider.Limits = [0 1];
            app.LowerSlider.MajorTicks = [];
            app.LowerSlider.ValueChangedFcn = createCallbackFcn(app, @SliderValueChanged, true);
            app.LowerSlider.MinorTicks = [0 0.0125 0.025 0.0375 0.05 0.0625 0.075 0.0875 0.1 0.1125 0.125 0.1375 0.15 0.1625 0.175 0.1875 0.2 0.2125 0.225 0.2375 0.25 0.2625 0.275 0.2875 0.3 0.3125 0.325 0.3375 0.35 0.3625 0.375 0.3875 0.4 0.4125 0.425 0.4375 0.45 0.4625 0.475 0.4875 0.5 0.5125 0.525 0.5375 0.55 0.5625 0.575 0.5875 0.6 0.6125 0.625 0.6375 0.65 0.6625 0.675 0.6875 0.7 0.7125 0.725 0.7375 0.75 0.7625 0.775 0.7875 0.8 0.8125 0.825 0.8375 0.85 0.8625 0.875 0.8875 0.9 0.9125 0.925 0.9375 0.95 0.9625 0.975 0.9875 1];
            app.LowerSlider.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.LowerSlider.Position = [12 47 295 3];

            % Create UpperSlider
            app.UpperSlider = uislider(app.ContrastAdjustmentPanel);
            app.UpperSlider.Limits = [0 1];
            app.UpperSlider.MajorTicks = [];
            app.UpperSlider.ValueChangedFcn = createCallbackFcn(app, @SliderValueChanged, true);
            app.UpperSlider.MinorTicks = [0 0.0125 0.025 0.0375 0.05 0.0625 0.075 0.0875 0.1 0.1125 0.125 0.1375 0.15 0.1625 0.175 0.1875 0.2 0.2125 0.225 0.2375 0.25 0.2625 0.275 0.2875 0.3 0.3125 0.325 0.3375 0.35 0.3625 0.375 0.3875 0.4 0.4125 0.425 0.4375 0.45 0.4625 0.475 0.4875 0.5 0.5125 0.525 0.5375 0.55 0.5625 0.575 0.5875 0.6 0.6125 0.625 0.6375 0.65 0.6625 0.675 0.6875 0.7 0.7125 0.725 0.7375 0.75 0.7625 0.775 0.7875 0.8 0.8125 0.825 0.8375 0.85 0.8625 0.875 0.8875 0.9 0.9125 0.925 0.9375 0.95 0.9625 0.975 0.9875 1];
            app.UpperSlider.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.UpperSlider.Position = [12 121 295 3];
            app.UpperSlider.Value = 1;

            % Create contrastManualButton
            app.contrastManualButton = uibutton(app.ContrastAdjustmentPanel, 'state');
            app.contrastManualButton.ValueChangedFcn = createCallbackFcn(app, @ContrastAdjustment, true);
            app.contrastManualButton.Tooltip = {'Manual contrast adjustment'};
            app.contrastManualButton.Icon = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'manual.png');
            app.contrastManualButton.HorizontalAlignment = 'left';
            app.contrastManualButton.Text = 'Manual';
            app.contrastManualButton.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.contrastManualButton.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.contrastManualButton.Position = [11 10 75 26];

            % Create contrastAutoButton
            app.contrastAutoButton = uibutton(app.ContrastAdjustmentPanel, 'state');
            app.contrastAutoButton.ValueChangedFcn = createCallbackFcn(app, @ContrastAdjustment, true);
            app.contrastAutoButton.Tooltip = {'Auto contrast adjustment'};
            app.contrastAutoButton.Icon = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'auto.png');
            app.contrastAutoButton.HorizontalAlignment = 'left';
            app.contrastAutoButton.Text = 'Auto';
            app.contrastAutoButton.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.contrastAutoButton.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.contrastAutoButton.Position = [96 10 59 26];
            app.contrastAutoButton.Value = true;

            % Create contrastContourButton
            app.contrastContourButton = uibutton(app.ContrastAdjustmentPanel, 'state');
            app.contrastContourButton.ValueChangedFcn = createCallbackFcn(app, @ContrastAdjustment, true);
            app.contrastContourButton.Tooltip = {'Contrast based on a region around the contour'};
            app.contrastContourButton.Icon = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'contour.png');
            app.contrastContourButton.HorizontalAlignment = 'left';
            app.contrastContourButton.Text = 'Contour';
            app.contrastContourButton.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.contrastContourButton.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.contrastContourButton.Position = [165 10 78 26];

            % Create contrastROIButton
            app.contrastROIButton = uibutton(app.ContrastAdjustmentPanel, 'state');
            app.contrastROIButton.ValueChangedFcn = createCallbackFcn(app, @ContrastAdjustment, true);
            app.contrastROIButton.Tooltip = {'Contrast based on a selected region of interest'};
            app.contrastROIButton.Icon = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'ROI.png');
            app.contrastROIButton.HorizontalAlignment = 'left';
            app.contrastROIButton.Text = 'ROI';
            app.contrastROIButton.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.contrastROIButton.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.contrastROIButton.Position = [253 10 56 26];

            % Create ContourAlignmentPanel
            app.ContourAlignmentPanel = uipanel(app.GridLayout);
            app.ContourAlignmentPanel.Enable = 'off';
            app.ContourAlignmentPanel.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ContourAlignmentPanel.BorderType = 'none';
            app.ContourAlignmentPanel.Title = 'Contour Alignment';
            app.ContourAlignmentPanel.BackgroundColor = [0.902 0.902 0.902];
            app.ContourAlignmentPanel.Layout.Row = 2;
            app.ContourAlignmentPanel.Layout.Column = 4;
            app.ContourAlignmentPanel.FontWeight = 'bold';
            app.ContourAlignmentPanel.FontSize = 12.5;

            % Create E
            app.E = uiimage(app.ContourAlignmentPanel);
            app.E.ImageClickedFcn = createCallbackFcn(app, @E_ButtonPushed, true);
            app.E.Position = [95 91 32 32];
            app.E.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'east.png');

            % Create N
            app.N = uiimage(app.ContourAlignmentPanel);
            app.N.ImageClickedFcn = createCallbackFcn(app, @N_ButtonPushed, true);
            app.N.Position = [57 129 32 32];
            app.N.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'north.png');

            % Create NE
            app.NE = uiimage(app.ContourAlignmentPanel);
            app.NE.ImageClickedFcn = createCallbackFcn(app, @NE_ButtonPushed, true);
            app.NE.Position = [95 129 32 32];
            app.NE.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'northeast.png');

            % Create SE
            app.SE = uiimage(app.ContourAlignmentPanel);
            app.SE.ImageClickedFcn = createCallbackFcn(app, @SE_ButtonPushed, true);
            app.SE.Position = [95 53 32 32];
            app.SE.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'southeast.png');

            % Create S
            app.S = uiimage(app.ContourAlignmentPanel);
            app.S.ImageClickedFcn = createCallbackFcn(app, @S_ButtonPushed, true);
            app.S.Position = [57 53 32 32];
            app.S.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'south.png');

            % Create W
            app.W = uiimage(app.ContourAlignmentPanel);
            app.W.ImageClickedFcn = createCallbackFcn(app, @W_ButtonPushed, true);
            app.W.Position = [19 91 32 32];
            app.W.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'west.png');

            % Create NW
            app.NW = uiimage(app.ContourAlignmentPanel);
            app.NW.ImageClickedFcn = createCallbackFcn(app, @NW_ButtonPushed, true);
            app.NW.Position = [19 129 32 32];
            app.NW.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'northwest.png');

            % Create SW
            app.SW = uiimage(app.ContourAlignmentPanel);
            app.SW.ImageClickedFcn = createCallbackFcn(app, @SW_ButtonPushed, true);
            app.SW.Position = [19 53 32 32];
            app.SW.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'southwest.png');

            % Create ConfidenceButtonGroup
            app.ConfidenceButtonGroup = uibuttongroup(app.ContourAlignmentPanel);
            app.ConfidenceButtonGroup.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ConfidenceButtonGroup.Title = 'Confidence in alignment';
            app.ConfidenceButtonGroup.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.ConfidenceButtonGroup.FontWeight = 'bold';
            app.ConfidenceButtonGroup.Position = [146 9 168 152];

            % Create C5Button
            app.C5Button = uiradiobutton(app.ConfidenceButtonGroup);
            app.C5Button.Text = {'5: Very confident'; ''};
            app.C5Button.FontWeight = 'bold';
            app.C5Button.FontColor = [0.3098 0.8 0];
            app.C5Button.Position = [11 5 119 22];

            % Create C4Button
            app.C4Button = uiradiobutton(app.ConfidenceButtonGroup);
            app.C4Button.Text = '4: Fairly confident';
            app.C4Button.FontWeight = 'bold';
            app.C4Button.FontColor = [0.549 0.9216 0];
            app.C4Button.Position = [11 26 126 22];

            % Create C3Button
            app.C3Button = uiradiobutton(app.ConfidenceButtonGroup);
            app.C3Button.Text = '3: Neither';
            app.C3Button.FontWeight = 'bold';
            app.C3Button.FontColor = [1 0.8078 0.0118];
            app.C3Button.Position = [11 46 78 22];

            % Create C2Button
            app.C2Button = uiradiobutton(app.ConfidenceButtonGroup);
            app.C2Button.Text = '2: Not very confident';
            app.C2Button.FontWeight = 'bold';
            app.C2Button.FontColor = [0.9882 0.3804 0.0196];
            app.C2Button.Position = [11 66 142 22];

            % Create C1Button
            app.C1Button = uiradiobutton(app.ConfidenceButtonGroup);
            app.C1Button.Text = '1: Not at all confident';
            app.C1Button.FontWeight = 'bold';
            app.C1Button.FontColor = [0.9412 0.0196 0.0196];
            app.C1Button.Position = [11 86 144 22];

            % Create C0Button
            app.C0Button = uiradiobutton(app.ConfidenceButtonGroup);
            app.C0Button.Text = '0: Unlabelled';
            app.C0Button.FontWeight = 'bold';
            app.C0Button.FontColor = [0.149 0.149 0.149];
            app.C0Button.Position = [11 106 142 22];
            app.C0Button.Value = true;

            % Create reset
            app.reset = uiimage(app.ContourAlignmentPanel);
            app.reset.ImageClickedFcn = createCallbackFcn(app, @ResetContourButtonPushed, true);
            app.reset.Tooltip = {'Reset contour position'};
            app.reset.Position = [59 93 28 28];
            app.reset.ImageSource = fullfile(pathToMLAPP, 'ContourAlignmentTool_resources', 'reset.png');

            % Create position
            app.position = uilabel(app.ContourAlignmentPanel);
            app.position.HorizontalAlignment = 'center';
            app.position.FontSize = 18;
            app.position.FontWeight = 'bold';
            app.position.FontColor = [0.149 0.149 0.149];
            app.position.Position = [1 8 145 22];
            app.position.Text = '(0, 0)';

            % Create clickdragstatus
            app.clickdragstatus = uilabel(app.ContourAlignmentPanel);
            app.clickdragstatus.FontWeight = 'bold';
            app.clickdragstatus.FontColor = [0.3098 0.8 0];
            app.clickdragstatus.Tooltip = {''};
            app.clickdragstatus.Position = [105 29 25 22];
            app.clickdragstatus.Text = 'ON';

            % Create ClickdragCheckBox
            app.ClickdragCheckBox = uicheckbox(app.ContourAlignmentPanel);
            app.ClickdragCheckBox.ValueChangedFcn = createCallbackFcn(app, @ClicknDrag, true);
            app.ClickdragCheckBox.Text = 'Click & drag';
            app.ClickdragCheckBox.FontColor = [0.149 0.149 0.149];
            app.ClickdragCheckBox.Position = [19 29 87 22];
            app.ClickdragCheckBox.Value = true;

            % Create DataProcessingPanel
            app.DataProcessingPanel = uipanel(app.GridLayout);
            app.DataProcessingPanel.AutoResizeChildren = 'off';
            app.DataProcessingPanel.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.DataProcessingPanel.BorderType = 'none';
            app.DataProcessingPanel.Title = 'Data Processing';
            app.DataProcessingPanel.BackgroundColor = [0.9412 0.9412 0.9412];
            app.DataProcessingPanel.SizeChangedFcn = createCallbackFcn(app, @DataProcessingPanelSizeChanged, true);
            app.DataProcessingPanel.Layout.Row = [1 4];
            app.DataProcessingPanel.Layout.Column = [1 3];
            app.DataProcessingPanel.FontWeight = 'bold';
            app.DataProcessingPanel.FontSize = 12.5;

            % Create DirectoriesImage
            app.DirectoriesImage = uiimage(app.DataProcessingPanel);
            app.DirectoriesImage.ImageClickedFcn = createCallbackFcn(app, @DirectoriesImageClicked, true);
            app.DirectoriesImage.Position = [8 41 477 536];

            % Create StartProcessing
            app.StartProcessing = uibutton(app.DataProcessingPanel, 'push');
            app.StartProcessing.ButtonPushedFcn = createCallbackFcn(app, @StartProcessingButtonPushed, true);
            app.StartProcessing.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.StartProcessing.FontSize = 14;
            app.StartProcessing.FontWeight = 'bold';
            app.StartProcessing.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.StartProcessing.Enable = 'off';
            app.StartProcessing.Position = [689 17 100 33];
            app.StartProcessing.Text = 'Proceed';

            % Create parametersWarning
            app.parametersWarning = uilabel(app.DataProcessingPanel);
            app.parametersWarning.VerticalAlignment = 'top';
            app.parametersWarning.WordWrap = 'on';
            app.parametersWarning.FontColor = [1 0.4118 0.1608];
            app.parametersWarning.Visible = 'off';
            app.parametersWarning.Position = [482 233 318 42];
            app.parametersWarning.Text = 'Warning: unable to determine the pixel spacing, SID, SDD, and x-offset. Confirm these values before proceeding.';

            % Create kVImagingParametersPanel
            app.kVImagingParametersPanel = uipanel(app.DataProcessingPanel);
            app.kVImagingParametersPanel.AutoResizeChildren = 'off';
            app.kVImagingParametersPanel.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.kVImagingParametersPanel.Title = 'kV Imaging Parameters';
            app.kVImagingParametersPanel.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.kVImagingParametersPanel.FontWeight = 'bold';
            app.kVImagingParametersPanel.Position = [479 276 321 284];

            % Create SourcetoisocenterdistanceSIDmmEditFieldLabel
            app.SourcetoisocenterdistanceSIDmmEditFieldLabel = uilabel(app.kVImagingParametersPanel);
            app.SourcetoisocenterdistanceSIDmmEditFieldLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.SourcetoisocenterdistanceSIDmmEditFieldLabel.Position = [28 187 278 22];
            app.SourcetoisocenterdistanceSIDmmEditFieldLabel.Text = 'Source to isocenter distance (SID)                    mm';

            % Create SourcetodetectordistanceSDDmmLabel
            app.SourcetodetectordistanceSDDmmLabel = uilabel(app.kVImagingParametersPanel);
            app.SourcetodetectordistanceSDDmmLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.SourcetodetectordistanceSDDmmLabel.Position = [28 162 278 22];
            app.SourcetodetectordistanceSDDmmLabel.Text = 'Source to detector distance (SDD)                    mm';

            % Create DetectoroffsetxcoordinatemmLabel
            app.DetectoroffsetxcoordinatemmLabel = uilabel(app.kVImagingParametersPanel);
            app.DetectoroffsetxcoordinatemmLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.DetectoroffsetxcoordinatemmLabel.Position = [28 87 278 22];
            app.DetectoroffsetxcoordinatemmLabel.Text = 'Detector offset x-coordinate                      l        mm';

            % Create offsetValue
            app.offsetValue = uieditfield(app.kVImagingParametersPanel, 'numeric');
            app.offsetValue.HorizontalAlignment = 'center';
            app.offsetValue.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.offsetValue.Position = [223 87 47 22];
            app.offsetValue.Value = 160;

            % Create NumberofimagestoloadLabel
            app.NumberofimagestoloadLabel = uilabel(app.kVImagingParametersPanel);
            app.NumberofimagestoloadLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.NumberofimagestoloadLabel.Position = [29 238 143 22];
            app.NumberofimagestoloadLabel.Text = 'Number of images to load';

            % Create PixelspacingmmLabel
            app.PixelspacingmmLabel = uilabel(app.kVImagingParametersPanel);
            app.PixelspacingmmLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.PixelspacingmmLabel.Position = [27 112 279 22];
            app.PixelspacingmmLabel.Text = 'Detector pixel spacing                                        mm';

            % Create offsetLamp
            app.offsetLamp = uilamp(app.kVImagingParametersPanel);
            app.offsetLamp.Position = [7 91 15 15];
            app.offsetLamp.Color = [0.902 0.902 0.902];

            % Create SDDvalue
            app.SDDvalue = uieditfield(app.kVImagingParametersPanel, 'numeric');
            app.SDDvalue.Limits = [0 Inf];
            app.SDDvalue.HorizontalAlignment = 'center';
            app.SDDvalue.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.SDDvalue.Position = [223 162 47 22];
            app.SDDvalue.Value = 1500;

            % Create SDDLamp
            app.SDDLamp = uilamp(app.kVImagingParametersPanel);
            app.SDDLamp.Position = [7 166 15 15];
            app.SDDLamp.Color = [0.902 0.902 0.902];

            % Create SIDvalue
            app.SIDvalue = uieditfield(app.kVImagingParametersPanel, 'numeric');
            app.SIDvalue.Limits = [0 Inf];
            app.SIDvalue.HorizontalAlignment = 'center';
            app.SIDvalue.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.SIDvalue.Position = [223 187 47 22];
            app.SIDvalue.Value = 1000;

            % Create SIDLamp
            app.SIDLamp = uilamp(app.kVImagingParametersPanel);
            app.SIDLamp.Position = [7 191 15 15];
            app.SIDLamp.Color = [0.902 0.902 0.902];

            % Create PixelSpacingValue
            app.PixelSpacingValue = uieditfield(app.kVImagingParametersPanel, 'numeric');
            app.PixelSpacingValue.Limits = [0 Inf];
            app.PixelSpacingValue.HorizontalAlignment = 'center';
            app.PixelSpacingValue.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.PixelSpacingValue.Position = [223 112 47 22];
            app.PixelSpacingValue.Value = 0.388;

            % Create PixelSpacingLamp
            app.PixelSpacingLamp = uilamp(app.kVImagingParametersPanel);
            app.PixelSpacingLamp.Position = [7 116 15 15];
            app.PixelSpacingLamp.Color = [0.902 0.902 0.902];

            % Create AllCheckBox
            app.AllCheckBox = uicheckbox(app.kVImagingParametersPanel);
            app.AllCheckBox.ValueChangedFcn = createCallbackFcn(app, @projectionsLoad, true);
            app.AllCheckBox.Text = 'All';
            app.AllCheckBox.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.AllCheckBox.Position = [239 238 35 22];

            % Create NumberProj
            app.NumberProj = uispinner(app.kVImagingParametersPanel);
            app.NumberProj.Limits = [1 1000];
            app.NumberProj.HorizontalAlignment = 'left';
            app.NumberProj.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.NumberProj.Position = [174 238 58 22];
            app.NumberProj.Value = 35;

            % Create StopAngleLabel
            app.StopAngleLabel = uilabel(app.kVImagingParametersPanel);
            app.StopAngleLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.StopAngleLabel.Position = [27 37 274 22];
            app.StopAngleLabel.Text = 'Detector stop angle                             r              °   ';

            % Create StopAngleValue
            app.StopAngleValue = uieditfield(app.kVImagingParametersPanel, 'numeric');
            app.StopAngleValue.Limits = [0 360];
            app.StopAngleValue.ValueDisplayFormat = '%.2f';
            app.StopAngleValue.HorizontalAlignment = 'center';
            app.StopAngleValue.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.StopAngleValue.Position = [223 37 47 22];
            app.StopAngleValue.Value = 360;

            % Create StopAngleLamp
            app.StopAngleLamp = uilamp(app.kVImagingParametersPanel);
            app.StopAngleLamp.Position = [7 41 15 15];
            app.StopAngleLamp.Color = [0.902 0.902 0.902];

            % Create StartAngleLabel
            app.StartAngleLabel = uilabel(app.kVImagingParametersPanel);
            app.StartAngleLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.StartAngleLabel.Position = [27 62 264 22];
            app.StartAngleLabel.Text = 'Detector start angle                                            °';

            % Create StartAngleValue
            app.StartAngleValue = uieditfield(app.kVImagingParametersPanel, 'numeric');
            app.StartAngleValue.Limits = [-360 360];
            app.StartAngleValue.ValueDisplayFormat = '%.2f';
            app.StartAngleValue.HorizontalAlignment = 'center';
            app.StartAngleValue.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.StartAngleValue.Position = [223 62 47 22];

            % Create StartAngleLamp
            app.StartAngleLamp = uilamp(app.kVImagingParametersPanel);
            app.StartAngleLamp.Position = [7 66 15 15];
            app.StartAngleLamp.Color = [0.902 0.902 0.902];

            % Create Label
            app.Label = uilabel(app.kVImagingParametersPanel);
            app.Label.Position = [27 11 125 22];
            app.Label.Text = '';

            % Create CollimatorCassetteDropDown
            app.CollimatorCassetteDropDown = uidropdown(app.kVImagingParametersPanel);
            app.CollimatorCassetteDropDown.Items = {'None', 'CTDI-S', 'CTDI-M', 'CTDI-L', 'L2', 'L10', 'L20', 'M2', 'M10', 'M15', 'M20', 'S10', 'S20', '15 x 15'};
            app.CollimatorCassetteDropDown.ItemsData = {'[]', '[425.0 30.7]', '[425.0 30.7]', '[425.0 30.7]', '[425.0 56.0]', '[425.0 220.0]', '[425.0 425.0]', '[425.0 54.0]', '[425.0 208.0]', '[425.0 274.2]', '[425.0 425.0]', '[425.0 208.0]', '[425.0 425.0]', '[245.8 245.8]'};
            app.CollimatorCassetteDropDown.Placeholder = 'None';
            app.CollimatorCassetteDropDown.Position = [171 11 99 22];
            app.CollimatorCassetteDropDown.Value = '[]';

            % Create MatrixSizeLabel
            app.MatrixSizeLabel = uilabel(app.kVImagingParametersPanel);
            app.MatrixSizeLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.MatrixSizeLabel.Position = [27 137 288 22];
            app.MatrixSizeLabel.Text = 'Detector matrix size                                           pixels';

            % Create MatrixSizeLamp
            app.MatrixSizeLamp = uilamp(app.kVImagingParametersPanel);
            app.MatrixSizeLamp.Position = [7 141 15 15];
            app.MatrixSizeLamp.Color = [0.902 0.902 0.902];

            % Create MatrixSizeDropDown
            app.MatrixSizeDropDown = uidropdown(app.kVImagingParametersPanel);
            app.MatrixSizeDropDown.Items = {'512x512', '768x1024', '1024x1024'};
            app.MatrixSizeDropDown.ItemsData = {'[512 512]', '[768 1024]', '[1024 1024]'};
            app.MatrixSizeDropDown.Placeholder = 'None';
            app.MatrixSizeDropDown.Position = [171 137 99 22];
            app.MatrixSizeDropDown.Value = '[512 512]';

            % Create MachineDropDownLabel
            app.MachineDropDownLabel = uilabel(app.kVImagingParametersPanel);
            app.MachineDropDownLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.MachineDropDownLabel.Position = [28 212 50 22];
            app.MachineDropDownLabel.Text = 'Machine';

            % Create MachineDropDownLamp
            app.MachineDropDownLamp = uilamp(app.kVImagingParametersPanel);
            app.MachineDropDownLamp.Position = [7 216 15 15];
            app.MachineDropDownLamp.Color = [0.902 0.902 0.902];

            % Create MachineDropDown
            app.MachineDropDown = uidropdown(app.kVImagingParametersPanel);
            app.MachineDropDown.Items = {'Elekta', 'Varian'};
            app.MachineDropDown.ValueChangedFcn = createCallbackFcn(app, @OnMachineDropDownChanged, true);
            app.MachineDropDown.Position = [171 212 99 22];
            app.MachineDropDown.Value = 'Elekta';

            % Create CollimatorCassetteDropDownLabel
            app.CollimatorCassetteDropDownLabel = uilabel(app.kVImagingParametersPanel);
            app.CollimatorCassetteDropDownLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.CollimatorCassetteDropDownLabel.Position = [27 11 107 22];
            app.CollimatorCassetteDropDownLabel.Text = 'Collimator cassette';

            % Create ExportLabel
            app.ExportLabel = uilabel(app.DataProcessingPanel);
            app.ExportLabel.FontColor = [0.502 0.502 0.502];
            app.ExportLabel.Tooltip = {''};
            app.ExportLabel.Position = [310 40 347 22];
            app.ExportLabel.Text = '(optional)';

            % Create ExportBrowse
            app.ExportBrowse = uibutton(app.DataProcessingPanel, 'push');
            app.ExportBrowse.ButtonPushedFcn = createCallbackFcn(app, @ExportBrowsePushed, true);
            app.ExportBrowse.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.ExportBrowse.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ExportBrowse.Position = [448 60 55 21];
            app.ExportBrowse.Text = {'Browse'; ''};

            % Create ProjectionsLabel
            app.ProjectionsLabel = uilabel(app.DataProcessingPanel);
            app.ProjectionsLabel.FontColor = [1 0.4118 0.1608];
            app.ProjectionsLabel.Position = [310 93 224 22];
            app.ProjectionsLabel.Text = 'Intrafraction images folder not selected';

            % Create ProjectionsBrowse
            app.ProjectionsBrowse = uibutton(app.DataProcessingPanel, 'push');
            app.ProjectionsBrowse.ButtonPushedFcn = createCallbackFcn(app, @ProjectionsBrowsePushed, true);
            app.ProjectionsBrowse.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.ProjectionsBrowse.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ProjectionsBrowse.Position = [479 112 55 21];
            app.ProjectionsBrowse.Text = {'Browse'; ''};

            % Create PlanLabel
            app.PlanLabel = uilabel(app.DataProcessingPanel);
            app.PlanLabel.FontColor = [1 0.4118 0.1608];
            app.PlanLabel.Position = [261 295 188 22];
            app.PlanLabel.Text = 'Plan file not selected';

            % Create PlanBrowse
            app.PlanBrowse = uibutton(app.DataProcessingPanel, 'push');
            app.PlanBrowse.ButtonPushedFcn = createCallbackFcn(app, @PlanBrowsePushed, true);
            app.PlanBrowse.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.PlanBrowse.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.PlanBrowse.Position = [328 315 55 21];
            app.PlanBrowse.Text = {'Browse'; ''};

            % Create SelectStructureStatus
            app.SelectStructureStatus = uilabel(app.DataProcessingPanel);
            app.SelectStructureStatus.BackgroundColor = [1 0.4118 0.1608];
            app.SelectStructureStatus.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.SelectStructureStatus.Visible = 'off';
            app.SelectStructureStatus.Position = [260 223 145 24];
            app.SelectStructureStatus.Text = '';

            % Create SelectStructure
            app.SelectStructure = uidropdown(app.DataProcessingPanel);
            app.SelectStructure.Items = {'Select structure'};
            app.SelectStructure.ValueChangedFcn = createCallbackFcn(app, @StructureSelected, true);
            app.SelectStructure.Visible = 'off';
            app.SelectStructure.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.SelectStructure.BackgroundColor = [0.9608 0.9608 0.9608];
            app.SelectStructure.Position = [261 224 143 22];
            app.SelectStructure.Value = 'Select structure';

            % Create StructureLabel
            app.StructureLabel = uilabel(app.DataProcessingPanel);
            app.StructureLabel.FontColor = [1 0.4118 0.1608];
            app.StructureLabel.Position = [261 244 192 22];
            app.StructureLabel.Text = 'Structure set file not selected';

            % Create StructureBrowse
            app.StructureBrowse = uibutton(app.DataProcessingPanel, 'push');
            app.StructureBrowse.ButtonPushedFcn = createCallbackFcn(app, @StructureBrowsePushed, true);
            app.StructureBrowse.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.StructureBrowse.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.StructureBrowse.Position = [398 264 55 21];
            app.StructureBrowse.Text = {'Browse'; ''};

            % Create CTLabel
            app.CTLabel = uilabel(app.DataProcessingPanel);
            app.CTLabel.FontColor = [1 0.4118 0.1608];
            app.CTLabel.Position = [261 346 172 22];
            app.CTLabel.Text = 'CT folder not selected';

            % Create CTBrowse
            app.CTBrowse = uibutton(app.DataProcessingPanel, 'push');
            app.CTBrowse.ButtonPushedFcn = createCallbackFcn(app, @CTBrowsePushed, true);
            app.CTBrowse.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.CTBrowse.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.CTBrowse.Position = [327 366 55 21];
            app.CTBrowse.Text = {'Browse'; ''};

            % Create FractionDropDown
            app.FractionDropDown = uidropdown(app.DataProcessingPanel);
            app.FractionDropDown.Items = {'Fraction'};
            app.FractionDropDown.ValueChangedFcn = createCallbackFcn(app, @OnFractionDropDownChanged, true);
            app.FractionDropDown.Enable = 'off';
            app.FractionDropDown.FontSize = 15;
            app.FractionDropDown.FontWeight = 'bold';
            app.FractionDropDown.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.FractionDropDown.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.FractionDropDown.Position = [263 146 140 25];
            app.FractionDropDown.Value = 'Fraction';

            % Create PatientDropDown
            app.PatientDropDown = uidropdown(app.DataProcessingPanel);
            app.PatientDropDown.Items = {'Patient', ''};
            app.PatientDropDown.ValueChangedFcn = createCallbackFcn(app, @OnPatientDropDownChanged, true);
            app.PatientDropDown.Enable = 'off';
            app.PatientDropDown.FontSize = 15;
            app.PatientDropDown.FontWeight = 'bold';
            app.PatientDropDown.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.PatientDropDown.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.PatientDropDown.Position = [159 438 140 25];
            app.PatientDropDown.Value = 'Patient';

            % Create MasterBrowse
            app.MasterBrowse = uibutton(app.DataProcessingPanel, 'push');
            app.MasterBrowse.ButtonPushedFcn = createCallbackFcn(app, @automatedSearch, true);
            app.MasterBrowse.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.MasterBrowse.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.MasterBrowse.Position = [44 475 55 21];
            app.MasterBrowse.Text = {'Browse'; ''};

            % Create ImagingTypeDropDownLabel
            app.ImagingTypeDropDownLabel = uilabel(app.DataProcessingPanel);
            app.ImagingTypeDropDownLabel.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ImagingTypeDropDownLabel.Enable = 'off';
            app.ImagingTypeDropDownLabel.Visible = 'off';
            app.ImagingTypeDropDownLabel.Position = [487 211 77 22];
            app.ImagingTypeDropDownLabel.Text = 'Imaging Type';

            % Create ImagingTypeDropDown
            app.ImagingTypeDropDown = uidropdown(app.DataProcessingPanel);
            app.ImagingTypeDropDown.Items = {'Kilovoltage', 'Megavoltage'};
            app.ImagingTypeDropDown.Enable = 'off';
            app.ImagingTypeDropDown.Visible = 'off';
            app.ImagingTypeDropDown.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ImagingTypeDropDown.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.ImagingTypeDropDown.Position = [633 211 97 22];
            app.ImagingTypeDropDown.Value = 'Kilovoltage';

            % Show the figure after all components are created
            app.ContourAlignmentToolUIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = ContourAlignmentTool

            % Create UIFigure and components
            createComponents(app)

            % Register the app with App Designer
            registerApp(app, app.ContourAlignmentToolUIFigure)

            % Execute the startup function
            runStartupFcn(app, @startupFcn)

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.ContourAlignmentToolUIFigure)
        end
    end
end