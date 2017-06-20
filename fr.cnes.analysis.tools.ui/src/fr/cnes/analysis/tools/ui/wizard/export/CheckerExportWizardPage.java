/************************************************************************************************/
/* i-Code CNES is a static code analyzer.                                                       */
/* This software is a free software, under the terms of the Eclipse Public License version 1.0. */
/* http://www.eclipse.org/legal/epl-v10.html                                                    */
/************************************************************************************************/
package fr.cnes.analysis.tools.ui.wizard.export;

import fr.cnes.analysis.tools.export.Export;
import fr.cnes.analysis.tools.export.exception.NoContributorMatchingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.TreeMap;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * This class is the main page of the {@link CheckerExportWizard}. It's
 * responsible of suggesting available formats for export to the user using
 * {@link Export#getAvailableFormats()} and indicating the chosen format to the
 * next page {@link CheckerFileCreationExportWizardPage}.
 * 
 * @version 3.0
 * @since 2.0
 * 
 * 
 */
public class CheckerExportWizardPage extends WizardPage {

    /**
     * Exporter service in charge of the analysis
     */
    private Export exporter;

    /**
     * Buttons list of all buttons offering available format for exportation.
     */
    private List<Button> formatButtons;

    /**
     * Map with Label and Text value of parameters set by the user (or default)
     */
    private Map<Label, Text> parametersFields;

    /** Information text notification about parameters */
    private Label parametersIndicator;

    /**
     * Create the wizard.
     * 
     * @param pSelection
     * @param exporter
     *            service.
     */
    public CheckerExportWizardPage(IStructuredSelection pSelection, Export exporter) {
        super("RuleExportWizardPage");
        this.setTitle("i-Code CNES - Analysis result export.");
        this.setDescription(
                "Description : Please choose the format of the export of you file. \nNote: This export will contain both metric & rules analysis result.");
        this.exporter = exporter;
        formatButtons = new ArrayList<>();
        parametersFields = new HashMap<>();
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#canFlipToNextPage()
     */
    @Override
    public boolean canFlipToNextPage() {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.
     * widgets.Composite)
     */
    @Override
    public void createControl(Composite pParent) {
        final Composite container = new Composite(pParent, SWT.NULL);
        setControl(container);
        container.setLayout(new GridLayout(1, false));
        for (String export : exporter.getAvailableFormats().keySet()) {
            Button btn = new Button(container, SWT.RADIO);
            btn.setText(export);

            /*
             * A new listener is set to update format chosen in the next page.
             */
            btn.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    CheckerFileCreationExportWizardPage nextPage = (CheckerFileCreationExportWizardPage) getWizard()
                            .getPage("RuleCreationFileExportWizardPage");
                    nextPage.updateFormat(btn.getText());
                    this.updateParameters(nextPage);
                    container.layout();
                }

                public void updateParameters(CheckerFileCreationExportWizardPage nextPage) {
                    if (parametersIndicator != null && !parametersIndicator.isDisposed()) {
                        parametersIndicator.dispose();
                    }

                    for (Entry<Label, Text> field : parametersFields.entrySet()) {
                        field.getValue().dispose();
                        field.getKey().dispose();
                    }
                    parametersFields.clear();
                    try {
                        if (exporter
                                .hasParameters(exporter.getAvailableFormats().get(btn.getText()))) {
                            Map<String, String> params = exporter.getParameters(
                                    exporter.getAvailableFormats().get(btn.getText()));
                            parametersIndicator = new Label(container,
                                    SWT.WRAP | SWT.BORDER | SWT.LEFT);
                            if (params.size() == 1) {
                                parametersIndicator.setText(
                                        "Information : This export requires parameters, edit default parameter if necessary before reaching next page.");
                            } else {
                                parametersIndicator.setText(
                                        "Information : This export requires parameters, edit default parameters if necessary before reaching next page.");
                            }
                            final GridData data = new GridData(SWT.HORIZONTAL, SWT.TOP, true, false,
                                    1, 1);
                            parametersIndicator.setLayoutData(data);
                            for (String key : params.keySet()) {

                                final Label fieldName = new Label(container, SWT.NULL);
                                fieldName.setText(key);
                                final Text fieldInput = new Text(container, SWT.FILL);
                                final GridData fieldInputStyle = new GridData();
                                fieldInputStyle.horizontalAlignment = SWT.FILL;
                                fieldInputStyle.grabExcessHorizontalSpace = true;
                                fieldInput.setLayoutData(fieldInputStyle);
                                fieldInput.addModifyListener(new ModifyListener() {

                                    @Override
                                    public void modifyText(ModifyEvent e) {
                                        Map<String, String> params = new TreeMap<>();
                                        for (Entry<Label, Text> param : parametersFields
                                                .entrySet()) {
                                            params.put(param.getKey().getText(),
                                                    param.getValue().getText());
                                        }
                                        nextPage.updateParameters(params);

                                    }
                                });
                                if (params.get(key) != null) {
                                    fieldInput.setText(params.get(key));
                                }
                                parametersFields.put(fieldName, fieldInput);
                            }
                        }
                    } catch (NoContributorMatchingException e) {
                        e.printStackTrace();
                    }

                }
            });
            this.formatButtons.add(btn);
            container.getParent().pack();
            container.requestLayout();
        }

        if (this.formatButtons.size() > 0) {
            this.formatButtons.get(0).setSelection(true);
            CheckerFileCreationExportWizardPage nextPage = (CheckerFileCreationExportWizardPage) getWizard()
                    .getPage("RuleCreationFileExportWizardPage");
            nextPage.updateFormat(this.formatButtons.get(0).getText());
        }

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    @Override
    public IWizardPage getNextPage() {
        return this.getWizard().getPage("RuleCreationFileExportWizardPage");

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#isPageComplete()
     */
    @Override
    public boolean isPageComplete() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getPreviousPage()
     */
    @Override
    public IWizardPage getPreviousPage() {
        return null;
    }

}
