classdef DRRviewer < matlab.apps.AppBase

    % Properties that correspond to app components
    properties (Access = public)
        drrUIFigure              matlab.ui.Figure
        contrastROIButton        matlab.ui.control.StateButton
        contrastContourButton    matlab.ui.control.StateButton
        contrastAutoButton       matlab.ui.control.StateButton
        contrastManualButton     matlab.ui.control.StateButton
        GridLayout               matlab.ui.container.GridLayout
        ContrastAdjustmentPanel  matlab.ui.container.Panel
        UpperSlider              matlab.ui.control.Slider
        LowerSlider              matlab.ui.control.Slider
        Histogram                matlab.ui.control.UIAxes
        ROIselectionLabel        matlab.ui.control.Label
        UIAxes                   matlab.ui.control.UIAxes
    end

    
    properties (Access = private)
        CallingApp  % Main app
        displaySize
        checkers
        I
        ROIrange
    end
    
    methods (Access = public)
        
        % Update the DRR viewer
        function updateDRR(app)

            if strcmp(app.CallingApp.fileType,'.his')
                drrMask = imrotate(app.CallingApp.originalMasks(:,:,app.CallingApp.currentFrame),-90);
            else
                drrMask = permute(app.CallingApp.originalMasks(:,:,app.CallingApp.currentFrame),[2 1 3]); 
            end
            
            
            drrMask = imbinarize(drrMask,0);
            drrMaskOutline = boundarymask(drrMask);
            
            app.I = app.CallingApp.DRR;
                
            % Set the contrast of the projection
            if app.contrastAutoButton.Value || app.contrastManualButton.Value
            
                lower = prctile(app.I(:),0.1);
                upper = prctile(app.I(:),99);
                if app.contrastAutoButton.Value
                    app.LowerSlider.Value = (prctile(app.I(:),5)-lower)/(upper-lower);
                    app.UpperSlider.Value = (prctile(app.I(:),95)-lower)/(upper-lower);
                end

            elseif app.contrastROIButton.Value
               lower = app.ROIrange(1);
               upper = app.ROIrange(2);
               
            elseif app.contrastContourButton.Value
                s = regionprops(double(drrMask),'BoundingBox');
                s.BoundingBox = round(s.BoundingBox);
                
                windowsize = 100;
                windowposition(1) = s.BoundingBox(2)-windowsize/2;
                if windowposition(1) < 1
                    windowposition(1) = 1;
                end
                
                matrixSize = str2num(app.CallingApp.MatrixSizeDropDown.Value);
                windowposition(2) = s.BoundingBox(2)+s.BoundingBox(4)+windowsize/2;
                if windowposition(2) > matrixSize(1)
                    windowposition(2) = matrixSize(1);
                end
                
                windowposition(3) = s.BoundingBox(1)-windowsize/2;
                if windowposition(3) < 1
                    windowposition(3) = 1;
                end
                
                windowposition(4) =  s.BoundingBox(1)+s.BoundingBox(3)+windowsize/2;
                if windowposition(4) > matrixSize(2)
                    windowposition(4) = matrixSize(2);
                end

                lower = min(min(app.I(windowposition(1):windowposition(2),...
                    windowposition(3):windowposition(4))));
                upper = max(max(app.I(windowposition(1):windowposition(2),...
                    windowposition(3):windowposition(4))));                   
                        
            end
            
            val1 = app.LowerSlider.Value * (upper-lower)+lower;
            val2 = app.UpperSlider.Value * (upper-lower)+lower;
            histogram(app.Histogram,app.I,100, 'BinLimits',[lower,upper],'EdgeColor','none','FaceColor',[0.65,0.65,0.65]); 
            
                
            % Set the width of the DRR display
            matrixSize = str2num(app.CallingApp.MatrixSizeDropDown.Value);
            if app.UIAxes.Position(3)/app.UIAxes.Position(4) <= matrixSize(2)/matrixSize(1)
                app.displaySize(1) = app.UIAxes.Position(3);
                app.displaySize(2) = app.UIAxes.Position(3)/(matrixSize(2)/matrixSize(1));
            else
                app.displaySize(1) = app.UIAxes.Position(4)*(matrixSize(2)/matrixSize(1));
                app.displaySize(2) = app.UIAxes.Position(4);
            end                       
            
                
            % Display the DRR
            imshow(app.I,[min([val1,val2])*max(app.I(:)) max([val1,val2])*max(app.I(:))],'Parent',app.UIAxes,'XData', [1 app.displaySize(1)],'YData', [1 app.displaySize(2)]);
            app.UIAxes.XLim = [1 app.displaySize(1)];
            app.UIAxes.YLim = [1 app.displaySize(2)];
            
            % Display the original contour
            hold(app.UIAxes,'on')
                maskimage = imshow(app.checkers,'Parent',app.UIAxes,'XData', [1 app.displaySize(1)],'YData', [1 app.displaySize(2)]);
            hold(app.UIAxes,'off')
            set(maskimage, 'AlphaData', drrMaskOutline) 
            
            % Display the current contour
            hold(app.UIAxes,'on')
                maskimage = imshow(app.CallingApp.colour,'Parent',app.UIAxes,'XData', [1 app.displaySize(1)],'YData', [1 app.displaySize(2)]);
            hold(app.UIAxes,'off')
            if app.CallingApp.ContourFillMenu.Checked
                set(maskimage, 'AlphaData', app.CallingApp.Mask*0.25) 
            else
                set(maskimage, 'AlphaData', app.CallingApp.MaskOutline) 
            end

            % Show the margin.
            if app.CallingApp.mode == "selection"
                hold(app.UIAxes, 'on');
                [h, w, ~] = size(app.I);
                % Map margin from pixels to display.
                marginX = app.CallingApp.marginX * app.displaySize(1) / w;
                marginY = app.CallingApp.marginY * app.displaySize(2) / h;
                rectangle(app.UIAxes, ...
                    'Position', [marginX, marginY, app.displaySize(1) - 2 * marginX, app.displaySize(2) - 2 * marginY], ...
                    'EdgeColor', 'yellow', ...
                    'LineWidth', 1 );
            end
        end
    end
    

    % Callbacks that handle component events
    methods (Access = private)

        % Code that executes after component creation
        function startupFcn(app, caller)
            app.CallingApp = caller;
            
            % Set the location of the window
            left = app.CallingApp.ContourAlignmentToolUIFigure.Position(1)+app.CallingApp.ContourAlignmentToolUIFigure.Position(3)/2-app.drrUIFigure.Position(3)/2;
            bottom = app.CallingApp.ContourAlignmentToolUIFigure.Position(2)+app.CallingApp.ContourAlignmentToolUIFigure.Position(4)/2-app.drrUIFigure.Position(4)/2;
            movegui(app.drrUIFigure,[left bottom]);
            
            % Disable interactivity
            disableDefaultInteractivity(app.UIAxes)
            disableDefaultInteractivity(app.Histogram)
            
            % Create the image used for the original contour position
            % border
            matrixSize = str2num(app.CallingApp.MatrixSizeDropDown.Value);
            app.checkers = checkerboard(4,matrixSize(1)/8,matrixSize(2)/8)>0;
            app.checkers = double(cat(3,app.checkers,app.checkers,app.checkers)).*cat(3, ones(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)), zeros(matrixSize(1),matrixSize(2)));
        end

        % Close request function: drrUIFigure
        function drrUIFigureCloseRequest(app, event)
            delete(app)
        end

        % Value changed function: contrastAutoButton, 
        % ...and 3 other components
        function ContrastAdjustment(app, event)
            app.contrastManualButton.Value = 0;
            app.contrastAutoButton.Value = 0;
            app.contrastContourButton.Value = 0;
            app.contrastROIButton.Value = 0;
            
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
                    app.ROIselectionLabel.Visible = 'on';
                    
                    matrixSize = str2num(app.CallingApp.MatrixDropDown.Value);
                    ROI = drawrectangle(app.UIAxes);
                    ROI.Position(1) = round(ROI.Position(1)/app.displaySize(1)*matrixSize(2));
                    ROI.Position(3) = round(ROI.Position(3)/app.displaySize(1)*matrixSize(2));
                    ROI.Position(2) = round(ROI.Position(2)/app.displaySize(2)*matrixSize(1));
                    ROI.Position(4) = round(ROI.Position(4)/app.displaySize(2)*matrixSize(1));
                     
                    
                    app.LowerSlider.Value = 0;
                    app.UpperSlider.Value = 1;
                    
                    
                    app.ROIrange(1) = min(min(app.I(ROI.Position(2):ROI.Position(2)+ROI.Position(4),...
                        ROI.Position(1):ROI.Position(1)+ROI.Position(3))));
                    app.ROIrange(2) = max(max(app.I(ROI.Position(2):ROI.Position(2)+ROI.Position(4),...
                        ROI.Position(1):ROI.Position(1)+ROI.Position(3))));
                    
                    delete(ROI)
                    
                    app.ROIselectionLabel.Visible = 'off';
            end
            
            
            updateDRR(app)  
        end

        % Value changed function: LowerSlider, UpperSlider
        function SliderValueChanging(app, event)
            % Update the contrast if the slider is changed
             if app.contrastAutoButton.Value
                app.contrastManualButton.Value = 1;
                app.contrastAutoButton.Value = 0;
             end
            updateDRR(app)  
        end
    end

    % Component initialization
    methods (Access = private)

        % Create UIFigure and components
        function createComponents(app)

            % Create drrUIFigure and hide until all components are created
            app.drrUIFigure = uifigure('Visible', 'off');
            app.drrUIFigure.Color = [0.651 0.651 0.651];
            app.drrUIFigure.Position = [100 100 800 600];
            app.drrUIFigure.Name = 'DRR Viewer';
            app.drrUIFigure.Icon = 'icon_48.png';
            app.drrUIFigure.CloseRequestFcn = createCallbackFcn(app, @drrUIFigureCloseRequest, true);

            % Create GridLayout
            app.GridLayout = uigridlayout(app.drrUIFigure);
            app.GridLayout.ColumnWidth = {322, '1x'};
            app.GridLayout.RowHeight = {22, '3.22x', 137};
            app.GridLayout.ColumnSpacing = 0;
            app.GridLayout.RowSpacing = 0;
            app.GridLayout.Padding = [0 0 0 0];
            app.GridLayout.BackgroundColor = [0.651 0.651 0.651];

            % Create UIAxes
            app.UIAxes = uiaxes(app.GridLayout);
            app.UIAxes.Toolbar.Visible = 'off';
            app.UIAxes.XTick = [];
            app.UIAxes.XTickLabel = '';
            app.UIAxes.YTick = [];
            app.UIAxes.YTickLabel = '';
            app.UIAxes.Layout.Row = [1 3];
            app.UIAxes.Layout.Column = [1 2];
            app.UIAxes.HitTest = 'off';
            app.UIAxes.PickableParts = 'all';
            app.UIAxes.Visible = 'off';

            % Create ROIselectionLabel
            app.ROIselectionLabel = uilabel(app.GridLayout);
            app.ROIselectionLabel.HorizontalAlignment = 'center';
            app.ROIselectionLabel.FontColor = [0.851 0.3255 0.098];
            app.ROIselectionLabel.Visible = 'off';
            app.ROIselectionLabel.Layout.Row = 1;
            app.ROIselectionLabel.Layout.Column = [1 2];
            app.ROIselectionLabel.Text = 'Click and drag the mouse to draw the ROI for auto-contrast adjustment ';

            % Create ContrastAdjustmentPanel
            app.ContrastAdjustmentPanel = uipanel(app.GridLayout);
            app.ContrastAdjustmentPanel.ForegroundColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.ContrastAdjustmentPanel.BorderType = 'none';
            app.ContrastAdjustmentPanel.Title = 'Contrast Adjustment';
            app.ContrastAdjustmentPanel.BackgroundColor = [0.902 0.902 0.902];
            app.ContrastAdjustmentPanel.Layout.Row = 3;
            app.ContrastAdjustmentPanel.Layout.Column = 1;
            app.ContrastAdjustmentPanel.FontWeight = 'bold';
            app.ContrastAdjustmentPanel.FontSize = 12.5;

            % Create Histogram
            app.Histogram = uiaxes(app.ContrastAdjustmentPanel);
            app.Histogram.Toolbar.Visible = 'off';
            app.Histogram.XTick = [];
            app.Histogram.XTickLabel = '';
            app.Histogram.YTick = [];
            app.Histogram.YTickLabel = '';
            app.Histogram.Visible = 'off';
            app.Histogram.Position = [1 36 320 72];

            % Create LowerSlider
            app.LowerSlider = uislider(app.ContrastAdjustmentPanel);
            app.LowerSlider.Limits = [0 1];
            app.LowerSlider.MajorTicks = [];
            app.LowerSlider.ValueChangedFcn = createCallbackFcn(app, @SliderValueChanging, true);
            app.LowerSlider.MinorTicks = [0 0.0125 0.025 0.0375 0.05 0.0625 0.075 0.0875 0.1 0.1125 0.125 0.1375 0.15 0.1625 0.175 0.1875 0.2 0.2125 0.225 0.2375 0.25 0.2625 0.275 0.2875 0.3 0.3125 0.325 0.3375 0.35 0.3625 0.375 0.3875 0.4 0.4125 0.425 0.4375 0.45 0.4625 0.475 0.4875 0.5 0.5125 0.525 0.5375 0.55 0.5625 0.575 0.5875 0.6 0.6125 0.625 0.6375 0.65 0.6625 0.675 0.6875 0.7 0.7125 0.725 0.7375 0.75 0.7625 0.775 0.7875 0.8 0.8125 0.825 0.8375 0.85 0.8625 0.875 0.8875 0.9 0.9125 0.925 0.9375 0.95 0.9625 0.975 0.9875 1];
            app.LowerSlider.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.LowerSlider.Position = [12 39 295 3];

            % Create UpperSlider
            app.UpperSlider = uislider(app.ContrastAdjustmentPanel);
            app.UpperSlider.Limits = [0 1];
            app.UpperSlider.MajorTicks = [];
            app.UpperSlider.ValueChangedFcn = createCallbackFcn(app, @SliderValueChanging, true);
            app.UpperSlider.MinorTicks = [0 0.0125 0.025 0.0375 0.05 0.0625 0.075 0.0875 0.1 0.1125 0.125 0.1375 0.15 0.1625 0.175 0.1875 0.2 0.2125 0.225 0.2375 0.25 0.2625 0.275 0.2875 0.3 0.3125 0.325 0.3375 0.35 0.3625 0.375 0.3875 0.4 0.4125 0.425 0.4375 0.45 0.4625 0.475 0.4875 0.5 0.5125 0.525 0.5375 0.55 0.5625 0.575 0.5875 0.6 0.6125 0.625 0.6375 0.65 0.6625 0.675 0.6875 0.7 0.7125 0.725 0.7375 0.75 0.7625 0.775 0.7875 0.8 0.8125 0.825 0.8375 0.85 0.8625 0.875 0.8875 0.9 0.9125 0.925 0.9375 0.95 0.9625 0.975 0.9875 1];
            app.UpperSlider.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.UpperSlider.Position = [12 105 295 3];
            app.UpperSlider.Value = 1;

            % Create contrastManualButton
            app.contrastManualButton = uibutton(app.drrUIFigure, 'state');
            app.contrastManualButton.ValueChangedFcn = createCallbackFcn(app, @ContrastAdjustment, true);
            app.contrastManualButton.Tooltip = {'Manual contrast adjustment'};
            app.contrastManualButton.Icon = 'manual.png';
            app.contrastManualButton.HorizontalAlignment = 'left';
            app.contrastManualButton.Text = 'Manual';
            app.contrastManualButton.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.contrastManualButton.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.contrastManualButton.Position = [11 7 75 26];

            % Create contrastAutoButton
            app.contrastAutoButton = uibutton(app.drrUIFigure, 'state');
            app.contrastAutoButton.ValueChangedFcn = createCallbackFcn(app, @ContrastAdjustment, true);
            app.contrastAutoButton.Tooltip = {'Auto contrast adjustment'};
            app.contrastAutoButton.Icon = 'auto.png';
            app.contrastAutoButton.HorizontalAlignment = 'left';
            app.contrastAutoButton.Text = 'Auto';
            app.contrastAutoButton.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.contrastAutoButton.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.contrastAutoButton.Position = [96 7 59 26];
            app.contrastAutoButton.Value = true;

            % Create contrastContourButton
            app.contrastContourButton = uibutton(app.drrUIFigure, 'state');
            app.contrastContourButton.ValueChangedFcn = createCallbackFcn(app, @ContrastAdjustment, true);
            app.contrastContourButton.Tooltip = {'Contrast based on a region around the contour'};
            app.contrastContourButton.Icon = 'contour.png';
            app.contrastContourButton.HorizontalAlignment = 'left';
            app.contrastContourButton.Text = 'Contour';
            app.contrastContourButton.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.contrastContourButton.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.contrastContourButton.Position = [165 7 78 26];

            % Create contrastROIButton
            app.contrastROIButton = uibutton(app.drrUIFigure, 'state');
            app.contrastROIButton.ValueChangedFcn = createCallbackFcn(app, @ContrastAdjustment, true);
            app.contrastROIButton.Tooltip = {'Contrast based on a selected region of interest'};
            app.contrastROIButton.Icon = 'ROI.png';
            app.contrastROIButton.HorizontalAlignment = 'left';
            app.contrastROIButton.Text = 'ROI';
            app.contrastROIButton.BackgroundColor = [0.96078431372549 0.96078431372549 0.96078431372549];
            app.contrastROIButton.FontColor = [0.129411764705882 0.129411764705882 0.129411764705882];
            app.contrastROIButton.Position = [253 7 56 26];

            % Show the figure after all components are created
            app.drrUIFigure.Visible = 'on';
        end
    end

    % App creation and deletion
    methods (Access = public)

        % Construct app
        function app = DRRviewer(varargin)

            runningApp = getRunningApp(app);

            % Check for running singleton app
            if isempty(runningApp)

                % Create UIFigure and components
                createComponents(app)

                % Register the app with App Designer
                registerApp(app, app.drrUIFigure)

                % Execute the startup function
                runStartupFcn(app, @(app)startupFcn(app, varargin{:}))
            else

                % Focus the running singleton app
                figure(runningApp.drrUIFigure)

                app = runningApp;
            end

            if nargout == 0
                clear app
            end
        end

        % Code that executes before app deletion
        function delete(app)

            % Delete UIFigure when app is deleted
            delete(app.drrUIFigure)
        end
    end
end