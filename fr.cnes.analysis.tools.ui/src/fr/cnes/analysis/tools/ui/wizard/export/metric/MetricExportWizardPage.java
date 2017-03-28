package fr.cnes.analysis.tools.ui.wizard.export.metric;

import java.util.logging.Logger;

import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;

import fr.cnes.analysis.tools.ui.wizard.export.metric.csv.MetricCSVExportWizardPage;

/**
 * MetricExportWizardPage
 * 
 * @version 2.0
 * @since 2017-07-12
 * 
 *        This class page is the main one of the Metric Export Wizard. This
 *        class is requesting the user the export's format and defining the next
 *        page to call situationally.
 */
public class MetricExportWizardPage extends WizardPage {

    public static final Logger LOGGER = Logger.getLogger(MetricCSVExportWizardPage.class.getName());

    /** The radio button CSV */
    private Button btnCsv;
    /** The radio button XML */
    private Button btnXml;

    /**
     * Create the wizard.
     */
    public MetricExportWizardPage() {
        super("MetricExportWizardPage");
        setTitle("i-Code CNES - Metrics export");
        setDescription("Description : Choosing the type of result's export.");
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.
     * widgets.Composite)
     */
    @Override
    public void createControl(Composite parent) {
        final Composite container = new Composite(parent, SWT.NULL);

        setControl(container);
        container.setLayout(new GridLayout(2, false));

        btnCsv = new Button(container, SWT.RADIO);
        btnCsv.setText("CSV");

        btnXml = new Button(container, SWT.RADIO);
        btnXml.setText("XML");

    }

    /**
     * @return the btnCsv
     */
    public Button getBtnCsv() {
        return btnCsv;
    }

    /**
     * @return the btnXml
     */
    public Button getBtnXml() {
        return btnXml;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.wizard.WizardPage#getNextPage()
     */
    @Override
    public IWizardPage getNextPage() {
        IWizardPage nextPage;
        if (btnCsv.getSelection()) {
            nextPage = this.getWizard().getPage("MetricCSVExportWizardPage");
        } else {
            nextPage = this.getWizard().getPage("MetricXMLExportWizardPage");
        }
        return nextPage;

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
