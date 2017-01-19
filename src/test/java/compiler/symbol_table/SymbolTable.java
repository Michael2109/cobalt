package compiler.symbol_table;

import java.util.ArrayList;
import java.util.List;

public class SymbolTable {

    private static final SymbolTable SYMBOL_TABLE = new SymbolTable();
    public List<Row> rows;

    public SymbolTable() {
        rows = new ArrayList<>();
    }

    public static SymbolTable getInstance() {
        return SYMBOL_TABLE;
    }

    public void addRow(Row row) {
        rows.add(row);
    }

    public boolean exists(String name, String methodName, String className) {
        if (name == null) {
            return false;
        }

        for (Row r : rows) {
            if (r.getName() == null || r.getMethodName() == null || r.getClassName() == null) {
                continue;
            }
            if (r.getName().equals(name) && r.getMethodName().equals(methodName) && r.getClassName().equals(className)) {
                return true;
            }
        }

        // Check ifs variables declared in class scope exist
        for (Row r : rows) {
            if (methodName == null && r.getMethodName() == null && name != null && r.getName() != null) {
                if (name.equals(r.getName())) {

                    return true;
                }
            }
        }
        return false;
    }

    public String getType(Block block) {
        return block.getType();
    }

    public void printSymbols() {

        System.out.printf("%-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s \n", "|", "Name", "|", "Type", "|", "Value", "|", "Method", "|", "Class");
        System.out.printf("%-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s \n", "+", "----", "+", "----", "+", "----", "+", "----", "+", "----");
        for (Row row : rows) {
            System.out.printf("%-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s %-15s %-1s \n", " ", row.getName(), " ", row.getType(), " ", row.getValue(), " ", row.getMethodName(), " ", row.getClassName(), " ");
        }

    }

    public Row getValue(Block method, String variableName) {
        for (Row row : rows) {
            if (row.getMethodName() != null && row.getMethodName().equals(method.getName()) && row.getName() != null && row.getName().equals(variableName)) {
                return row;
            }
        }
        return null;
    }

}
