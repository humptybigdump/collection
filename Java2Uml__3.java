import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.LayoutManager;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.font.TextAttribute;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingConstants;
import javax.swing.Timer;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.EtchedBorder;
import javax.swing.border.MatteBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.sun.org.apache.bcel.internal.classfile.ClassFormatException;
import com.sun.org.apache.bcel.internal.classfile.ClassParser;
import com.sun.org.apache.bcel.internal.classfile.Field;
import com.sun.org.apache.bcel.internal.classfile.FieldOrMethod;
import com.sun.org.apache.bcel.internal.classfile.JavaClass;
import com.sun.org.apache.bcel.internal.classfile.Method;

/**
 * A program to convert compiled java code from a .class file to a UML class diagram. The program supports changing the
 * font size and has an option to toggle "always on top" drawing behavior.
 * 
 * Usage: Type in the name of the .class file you want to convert and press Enter or click the button.
 * Hovering over an entry displays the original java signature of the field / method.
 *
 * @author Dominik Klooz
 * @version 1.2
 */
@SuppressWarnings("serial")
public class Java2Uml extends Frame {
    private UmlPanel umlPanel;
    private JLabel statusLabel;
    protected Font standardFont = new Font(Font.DIALOG, Font.PLAIN, 14);
    protected Font standardFontSmall = standardFont.deriveFont(12f);
    private static final String NO_ERRORS_MESSAGE = "No errors occured.";
    private static final int STATUS_MESSAGE_TIMEOUT = 7000; // = 7s
    public static final String APP_TITLE = "Java 2 UML";

    public static void main(String[] args) {
        new Java2Uml();
    }
    
    /**
     * Constructor, sets up the UI.
     */
    public Java2Uml () {
        this.setupWindow();
        
        JPanel topPanel = new JPanel(new GridBagLayout());
        this.populateTopPanel(topPanel);
        this.add(topPanel);
        
        JPanel statusPanel = new JPanel();
        this.populateStatusPanel(statusPanel);
        this.add(statusPanel, BorderLayout.SOUTH);
        
        setVisible(true);
    }
    
    /**
     * Initializes window properties.
     */
    private void setupWindow() {
        this.setTitle(APP_TITLE);
        this.setSize(640, 480);
        this.setMinimumSize(new Dimension(320, 240));
        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                e.getWindow().dispose();
                System.exit(0);
            }
        });
    }
    
    /**
     * Adds UI components to the given {@code JPanel}.
     *
     * @param topPanel The panel to populate.
     */
    private void populateTopPanel(JPanel topPanel) {
        GridBagConstraints c = new GridBagConstraints();
        
        JButton button = new JButton("Generate UML Diagram");
        
        final PlaceholderTextField textField = new PlaceholderTextField();
        textField.setPlaceholder("Enter name of .class file to be examined...");
        textField.setPreferredSize(new Dimension(textField.getPreferredSize().width, button.getPreferredSize().height + 1));
        textField.setFont(standardFont);
        textField.setMargin(new Insets(0, 4, 0, 0)); // add spacing from left border
        ActionListener submitListener = new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                generateUmlDiagram(textField.getText());
            }
        };
        textField.addActionListener(submitListener);
        c.weightx = 0.8;
        c.fill = GridBagConstraints.HORIZONTAL;
        c.anchor = GridBagConstraints.NORTH;
        c.insets = new Insets(10, 10, 10, 4);
        topPanel.add(textField, c);
        
        button.setMaximumSize(new Dimension(100, 20));
        button.addActionListener(submitListener);
        c.weightx = 0.0;
        c.insets = new Insets(10, 0, 10, 10);
        topPanel.add(button, c);

        umlPanel = new UmlPanel(new GridBagLayout());
        c.gridy = 1;
        c.gridwidth = 2;
        c.fill = GridBagConstraints.BOTH;
        c.weighty = 1.0;
        c.weightx = 1.0;
        c.insets = new Insets(10, 10, 10, 10);
        topPanel.add(umlPanel, c);
    }
    
    /**
     * Adds UI components to the given {@code JPanel}.
     *
     * @param statusPanel The panel to populate.
     */
    private void populateStatusPanel(JPanel statusPanel) {
        statusPanel.setLayout(new BoxLayout(statusPanel, BoxLayout.X_AXIS));
        statusPanel.setBorder(BorderFactory.createEtchedBorder(EtchedBorder.LOWERED));
        statusLabel = new JLabel(NO_ERRORS_MESSAGE);
        statusLabel.setFont(standardFontSmall);
        statusLabel.setHorizontalAlignment(SwingConstants.LEFT);
        statusLabel.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 5));
        
        final JSpinner spinner = new JSpinner(new SpinnerNumberModel(14, 10, 30, 2));
        spinner.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent e) {
                umlPanel.changeFontSize((Integer) spinner.getValue());
                pack();
            }
            
        });
        spinner.setMaximumSize(spinner.getPreferredSize()); // prevent horizontal resizing.
        
        JLabel fontSizeLabel = new JLabel("Font Size");
        fontSizeLabel.setFont(standardFontSmall);
        
        final JCheckBox onTopCheckbox = new JCheckBox("Stay on top");
        onTopCheckbox.setFont(standardFontSmall);
        onTopCheckbox.setHorizontalTextPosition(SwingConstants.LEADING);
        onTopCheckbox.addItemListener(new ItemListener() {
            @Override
            public void itemStateChanged(ItemEvent e) {
                if (e.getStateChange() == ItemEvent.SELECTED) {
                    setAlwaysOnTop(true);
                } else {
                    setAlwaysOnTop(false);
                }
            }
        });
        
        statusPanel.add(statusLabel);
        statusPanel.add(Box.createHorizontalGlue());
        statusPanel.add(fontSizeLabel, Box.RIGHT_ALIGNMENT);
        statusPanel.add(Box.createHorizontalStrut(5));
        statusPanel.add(spinner);
        statusPanel.add(Box.createHorizontalStrut(15));
        statusPanel.add(onTopCheckbox, Box.RIGHT_ALIGNMENT);
        statusPanel.add(Box.createHorizontalStrut(3));
    }
    
    /**
     * Generates a new UML class diagram from the file specified by {@code fileName}.
     *
     * @param fileName The name of the .class-file to be processed.
     */
    public void generateUmlDiagram(String fileName) {
        JavaClass javaClass = readClassFile(fileName);
        if (javaClass != null) {
            umlPanel.reset();
            String className = javaClass.getClassName().replaceAll("[a-zA-Z0-9_\\-]*\\.", "");
            
            umlPanel.setClassName(javaClass);
            for (Field field : javaClass.getFields()) {
                umlPanel.addMember(new UmlMemberLabel(field, className));
            }
            for (Method method : javaClass.getMethods()) {
                umlPanel.addMember(new UmlMemberLabel(method, className));
            }
            this.pack();
        }
    }
    
    /**
     * Sets the status label at the bottom of the window to the given status message. These messages are shown for
     * {@code MESSAGE_TIMEOUT} milliseconds. Errors are tainted red.
     *
     * @param message The message to be displayed.
     * @param isError Flag indicating if the message is an error message.
     */
    public void setStatusMessage(String message, boolean isError) {
        if (isError) {
            statusLabel.setForeground(Color.RED);
            Timer timer = new Timer(STATUS_MESSAGE_TIMEOUT, new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    setStatusMessage(NO_ERRORS_MESSAGE, false);
                }
                
            });
            timer.setRepeats(false);
            timer.start(); 
        } else {
            statusLabel.setForeground(Color.BLACK);
        }
        statusLabel.setText(message);
    }
    
    /**
     * Convenience overload of {@code setStatusMessage(String, boolean)}. Assumes the message is an error.
     *
     * @param message The message to be displayed.
     */
    public void setStatusMessage(String message) {
        setStatusMessage(message, true);
    }
    
    /**
     * Tries to read the .class-file specified by {@code fileName}. If the file cannot be read, error messages are
     * shown.
     *
     * @param fileName The name of the file to be read.
     * @return The JavaClass instance of the given .class-file or {@value null} if there was an error.
     */
    private JavaClass readClassFile(String fileName) {
        if (!fileName.endsWith(".class")) {
            fileName += ".class";
        }
        boolean errorOccured = false;
        ClassParser classParser = null;
        try {
            classParser = new ClassParser(fileName);
        } catch (FileNotFoundException e) {
            setStatusMessage("No file named \"" + fileName + "\" found.");
            errorOccured = true;
        } catch (IOException e) {
            setStatusMessage("Couldn't read file \"" + fileName + "\".");
            errorOccured = true;
        }
        JavaClass javaClass = null;
        if (classParser != null) {
            try {
                javaClass = classParser.parse();
            } catch (ClassFormatException e ) {
                setStatusMessage("File \"" + fileName + "\" is not a valid .class-File.");
                errorOccured = true;
            } catch (IOException e) {
            	setStatusMessage("File \"" + fileName + "\" is not a valid .class-File.");
                errorOccured = true;
            }
        }
        if (!errorOccured) {
            setStatusMessage("File \"" + fileName + "\" loaded succesfully.", false);
        }
        return javaClass;
    }
    
    /**
     * Extension of Swing's {@code JTextField} which allows for a placeholder to be shown, when the user hasn't typed
     * anything in yet.
     *
     * @author Dominik Klooz
     * @version 1.0
     */
    class PlaceholderTextField extends JTextField {
        private String placeholder;
        
        public PlaceholderTextField() {
            super();
            this.addCustomFocusListener();
        }
        
        public String getPlaceholder() {
            return placeholder;
        }

        public void setPlaceholder(final String s) {
            placeholder = s;
        }
        
        private void addCustomFocusListener() {
            this.addFocusListener(new FocusListener() {

                @Override
                public void focusGained(FocusEvent e) {
                    repaint();
                }

                @Override
                public void focusLost(FocusEvent e) {
                    repaint();
                }
            });
        }
        
        @Override
        protected void paintComponent(java.awt.Graphics g) {
            super.paintComponent(g);

            if (getText().isEmpty()){
                Graphics2D g2 = (Graphics2D)g.create();
                g2.setBackground(Color.gray);
                g2.setFont(getFont().deriveFont(Font.ITALIC));
                g2.drawString(placeholder, getInsets().left, g.getFontMetrics().getMaxAscent() + getInsets().top + 1);
                g2.dispose();
            }
        }
    }
    
    
    /**
     * A {@code JPanel} containing a UML class diagram.
     *
     * @author Dominik Klooz
     * @version 1.2
     */
    private class UmlPanel extends JPanel {
        private JLabel className;
        private JPanel fieldPanel;
        private JPanel methodPanel;
        GridBagConstraints c = new GridBagConstraints();
        private ArrayList<JLabel> labelList = new ArrayList<JLabel>();
        
        public UmlPanel(LayoutManager layout) {
            super(layout);
            this.setupUi();
        }
        
        /**
         * Sets up the class name label and the two class member boxes.
         */
        private void setupUi() {
            this.setBackground(Color.WHITE);
            this.setBorder(BorderFactory.createLineBorder(Color.BLACK, 2));
            
            className = new JLabel();
            
            CompoundBorder panelBorder = new CompoundBorder(new EmptyBorder(0, 0, 5, 0), 
                                                            new MatteBorder(2, 0, 0, 0, Color.BLACK));
            
            fieldPanel = new JPanel();
            BoxLayout boxLayout = new BoxLayout(fieldPanel, BoxLayout.PAGE_AXIS);
            fieldPanel.setLayout(boxLayout);
            fieldPanel.setBackground(Color.WHITE);
            fieldPanel.setBorder(panelBorder);

            methodPanel = new JPanel();
            BoxLayout boxLayout2 = new BoxLayout(methodPanel, BoxLayout.PAGE_AXIS);
            methodPanel.setLayout(boxLayout2);
            methodPanel.setBackground(Color.WHITE);
            methodPanel.setBorder(panelBorder);
            
            c.gridx = 0;
            c.gridy = 0;
            c.fill = GridBagConstraints.HORIZONTAL;
            c.weightx = 1.0;
            c.weighty = 0.0;
            c.insets = new Insets(4, 0, 4, 0);
            this.add(className, c);
            
            c.gridy = 1;
            c.fill = GridBagConstraints.BOTH;
            c.weightx = 1.0;
            c.weighty = 1.0;
            c.insets = new Insets(0, 0, 0 ,0);
            this.add(fieldPanel, c);
            
            c.gridy = 2;
            c.fill = GridBagConstraints.BOTH;
            c.weightx = 1.0;
            c.weighty = 1.0;
            this.add(methodPanel, c);
        }
        
        /**
         * Resets the member boxes to allow repopulation with fresh data.
         */
        public void reset() {
            this.fieldPanel.removeAll();
            this.methodPanel.removeAll();
            this.c = new GridBagConstraints();
            this.labelList = new ArrayList<JLabel>();
            this.revalidate();
            this.repaint();
        }
        
        /**
         * Sets the class name label, respecting UML formatting.
         *
         * @param javaClass The javaClass instance of the current .class-file.
         */
        public void setClassName(JavaClass javaClass) {
            className.setFont(standardFont.deriveFont(Font.BOLD));
            className.setHorizontalAlignment(JLabel.CENTER);
            labelList.add(className);
            String name = javaClass.getClassName().replaceAll("[a-zA-Z0-9_\\-]*\\.", "");
            if (javaClass.isAbstract()) {
                className.setFont(className.getFont().deriveFont(Font.ITALIC + Font.BOLD));
            }
            if (javaClass.isInterface()) {
                name = "«interface» " + name;
            }
            className.setText(name);
        }
        
        /**
         * Resizes all text inside of the UML diagram.
         *
         * @param newSize The new font size to be applied.
         */
        public void changeFontSize(int newSize) {
            for (JLabel label : labelList) {
                label.setFont(label.getFont().deriveFont((float) newSize));
            }
        }
        
        /**
         * Method to add an entry to the corresponding class member box.
         *
         * @param member
         */
        public void addMember(UmlMemberLabel member) {
            if (member.isMethod()) {
                methodPanel.add(member);
            } else {
                fieldPanel.add(member);
            }
            labelList.add(member);
        }
    }
    
    
    /**
     * A {@code JLabel} that describes a class member, respecting UML class diagram specifications. The tooltip shows the
     * original signature from the java source code.
     *
     * @author Dominik Klooz
     * @version 1.1
     */
    private class UmlMemberLabel extends JLabel {
        private FieldOrMethod member;
        private String className;
        private boolean isMethod = false;
        private static final int SPACING = 8;
        
        public UmlMemberLabel(FieldOrMethod member, String className) {
            super();
            this.member = member;
            if (member instanceof Method) {
                this.isMethod = true;
            }
            this.className = className;
            this.generateLabel();
        }
        
        public boolean isMethod() {
            return isMethod;
        }
        
        /**
         * Initializes this label form the {@code FieldOrMethod} passed to the constructor. All the UML formatting happens
         * here.
         */
        @SuppressWarnings({ "rawtypes", "unchecked" })
        private void generateLabel() {
            this.setToolTipText(member.toString().replaceAll("[a-zA-Z0-9_\\-]*\\.", "").replace("<init>", className));
            this.setFont(standardFont);
            Map attributes = standardFont.getAttributes();
            StringBuilder umlString = new StringBuilder();
            umlString.append(getAccessModifier());
            if (member.getName().startsWith("<init>")) {
                umlString.append("«constructor» " + className);
            } else {
                umlString.append(member.getName());
            }
            if (this.isMethod()) {
                umlString.append(" ( ");
                com.sun.org.apache.bcel.internal.generic.Type[] parameterTypes = ((Method) member).getArgumentTypes();
                String delimiter = "";
                for (com.sun.org.apache.bcel.internal.generic.Type pType : parameterTypes) {
                    umlString.append(delimiter).append(pType.toString().replaceAll("[a-zA-Z0-9_\\-]*\\.", ""));
                    delimiter = ", ";
                }
                umlString.append(" ) ");
            }
            umlString.append(" : ");
            if (this.isMethod()) {
                Method method = (Method) member;
                if (!method.getName().startsWith("<init>")) {
                    umlString.append(method.getReturnType().toString().replaceAll("[a-zA-Z0-9_\\-]*\\.", ""));
                } else { // if method is constructor
                    umlString.delete(umlString.length() - 3, umlString.length() - 1); // remove ": "
                }
            } else {
                umlString.append(((Field) member).getType().toString().replaceAll("[a-zA-Z0-9_\\-]*\\.", ""));
            }
            if (member.isStatic()) {
                attributes.put(TextAttribute.UNDERLINE, TextAttribute.UNDERLINE_ON);
                this.setFont(standardFont.deriveFont(attributes));
            }
            if (member.isAbstract()) {
                this.setFont(standardFont.deriveFont(Font.ITALIC));
            }
            String text = umlString.toString();
            this.setText(text);
            this.setBorder(BorderFactory.createEmptyBorder(0, SPACING, 0, SPACING));
        }
        
        /**
         * @return A String containing the access modifier character followed by 2 whitespaces and {@value {frozen}} if the
         *          member is final.
         */
        private String getAccessModifier() {
            String umlModifier;
            if (member.isPrivate()) {
                umlModifier = "-  ";
            } else if (member.isPublic()) {
                umlModifier = "+  ";
            } else if (member.isProtected()) {
                umlModifier = "#  ";
            } else {
                umlModifier = "~  ";
            }
            if (member.isFinal()) {
                umlModifier += "{frozen} ";
            }
            return umlModifier;
        }
    }
}
