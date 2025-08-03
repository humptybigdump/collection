package de.paulh.smartui;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.ThreadLocalRandom;

public final class SmartUI extends JFrame {

    public static void main(String[] args) {
        new SmartUI();
    }

    private JPanel canvas;
    private int[] array = new int[0];

    public SmartUI() {
        setTitle("SmartUI - Array-Sorter");
        setSize(400, 400);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        Container root = getContentPane();
        root.setLayout(new BorderLayout());

        // Add buttons
        JPanel buttonBar = new JPanel(new FlowLayout());

        TextField lengthText = new TextField("35");
        lengthText.setPreferredSize(new Dimension(100, 30));
        buttonBar.add(lengthText);

        Button fillButton = new Button("Fill");
        fillButton.setPreferredSize(new Dimension(100, 30));
        fillButton.addActionListener(e -> {
            int length = Integer.parseInt(lengthText.getText());
            array = new int[length];

            for (int i = 0; i < length; i++) {
                array[i] = ThreadLocalRandom.current().nextInt(100) + 1;
            }

            canvas.repaint();
        });
        buttonBar.add(fillButton);

        Button sortButton = new Button("Sort");
        sortButton.setPreferredSize(new Dimension(100, 30));
        sortButton.addActionListener(e -> new Thread(() -> {
            int n = array.length;
            for (int i = 1; i < n; ++i) {
                int key = array[i];
                int j = i - 1;

                while (j >= 0 && array[j] > key) {
                    array[j + 1] = array[j];
                    j = j - 1;
                    try {
                        canvas.repaint();
                        Thread.sleep(10);
                    } catch (InterruptedException interruptedException) {
                        interruptedException.printStackTrace();
                    }
                }

                array[j + 1] = key;
            }
        }).start());
        buttonBar.add(sortButton);

        root.add(buttonBar, BorderLayout.PAGE_START);

        // Add canvas
        this.canvas = new JPanel() {

            @Override
            public void paintComponent(Graphics g) {
                g.setColor(Color.LIGHT_GRAY);
                g.fillRect(0, 0, 400, 400);

                g.setColor(Color.BLACK);
                g.drawLine(0, 200, 400, 200);

                g.setColor(Color.RED);
                for (int i = 0; i < array.length; i++) {
                    g.fillRect(i * 10 + 10, 200 - array[i], 5, array[i]);
                }
            }
        };

        root.add(canvas, BorderLayout.CENTER);

        setVisible(true);
    }
}