package de.ag.jrlang.test;

import net.sf.jasperreports.engine.*;
import net.sf.jasperreports.engine.design.*;
import net.sf.jasperreports.engine.type.*;
import java.awt.*;
import java.util.Map;


public class Noxml {
  public static void main(String[] args) {
    try {
      JasperDesign d = getJasperDesignMin();
      write(print(compile(d)), "/Users/frese/tmp/testJava.pdf");
    } catch (JRException e) {
      e.printStackTrace();
    }
  }
  
  public static JasperReport compile(JasperDesign jasperDesign) throws JRException
  {
    long start = System.currentTimeMillis();
    //JasperCompileManager.compileReportToFile(jasperDesign, "build/reports/NoXmlDesignReport.jasper");
    JasperReport r = JasperCompileManager.compileReport(jasperDesign);
    System.err.println("Compile time : " + (System.currentTimeMillis() - start));
    return r;
  }
  
  public static JasperPrint print(JasperReport r) throws JRException {
    Map<String,Object> p = new java.util.HashMap<java.lang.String,java.lang.Object>();
    p.put("Test", "Test");
    return JasperFillManager.fillReport(r, p, new net.sf.jasperreports.engine.JREmptyDataSource());
  }
  
  public static void write(JasperPrint p, String fname) throws JRException {
    JasperExportManager.exportReportToPdfFile(p, fname);
  }

  private static JasperDesign getJasperDesignMin() throws JRException
  {
    JasperDesign jasperDesign = new JasperDesign();
    jasperDesign.setName("NoXmlDesignReport");
    
    JRDesignStyle normalStyle = new JRDesignStyle();
    normalStyle.setName("Sans_Normal");
    normalStyle.setDefault(true);
    //normalStyle.setFontName("DejaVu Sans");
    normalStyle.setFontSize(12);
    normalStyle.setPdfFontName("Helvetica");
    normalStyle.setPdfEncoding("Cp1252");
    normalStyle.setPdfEmbedded(false);
    jasperDesign.addStyle(normalStyle);

    //Page header
    JRDesignBand band = new JRDesignBand();
    band.setHeight(20);
    JRDesignStaticText staticText = new JRDesignStaticText();
    staticText.setX(0);
    staticText.setY(0);
    staticText.setWidth(55);
    staticText.setHeight(15);
    staticText.setForecolor(Color.white);
    staticText.setBackcolor(new Color(0x33, 0x33, 0x33));
    staticText.setMode(ModeEnum.OPAQUE);
    //staticText.setHorizontalAlignment(HorizontalAlignEnum.CENTER);
    staticText.setStyle(normalStyle);
    staticText.setText("ID");
    band.addElement(staticText);
    
    jasperDesign.setPageHeader(band);

    return jasperDesign;
  }

  private static JasperDesign getJasperDesignBig() throws JRException
  {
    //JasperDesign
    JasperDesign jasperDesign = new JasperDesign();
    jasperDesign.setName("NoXmlDesignReport");
    jasperDesign.setPageWidth(595);
    jasperDesign.setPageHeight(842);
    jasperDesign.setColumnWidth(515);
    jasperDesign.setColumnSpacing(0);
    jasperDesign.setLeftMargin(40);
    jasperDesign.setRightMargin(40);
    jasperDesign.setTopMargin(50);
    jasperDesign.setBottomMargin(50);
    
    //Fonts
    JRDesignStyle normalStyle = new JRDesignStyle();
    normalStyle.setName("Sans_Normal");
    normalStyle.setDefault(true);
    //normalStyle.setFontName("DejaVu Sans");
    normalStyle.setFontSize(12);
    normalStyle.setPdfFontName("Helvetica");
    normalStyle.setPdfEncoding("Cp1252");
    normalStyle.setPdfEmbedded(false);
    jasperDesign.addStyle(normalStyle);

    JRDesignStyle boldStyle = new JRDesignStyle();
    boldStyle.setName("Sans_Bold");
    //boldStyle.setFontName("DejaVu Sans");
    boldStyle.setFontSize(12);
    boldStyle.setBold(true);
    boldStyle.setPdfFontName("Helvetica-Bold");
    boldStyle.setPdfEncoding("Cp1252");
    boldStyle.setPdfEmbedded(false);
    jasperDesign.addStyle(boldStyle);

    JRDesignStyle italicStyle = new JRDesignStyle();
    italicStyle.setName("Sans_Italic");
    //italicStyle.setFontName("DejaVu Sans");
    italicStyle.setFontSize(12);
    italicStyle.setItalic(true);
    italicStyle.setPdfFontName("Helvetica-Oblique");
    italicStyle.setPdfEncoding("Cp1252");
    italicStyle.setPdfEmbedded(false);
    jasperDesign.addStyle(italicStyle);
    
    //Parameters
    JRDesignParameter parameter = new JRDesignParameter();
    parameter.setName("ReportTitle");
    parameter.setValueClass(java.lang.String.class);
    jasperDesign.addParameter(parameter);

    parameter = new JRDesignParameter();
    parameter.setName("OrderByClause");
    parameter.setValueClass(java.lang.String.class);
    jasperDesign.addParameter(parameter);

    //Query
    JRDesignQuery query = new JRDesignQuery();
    query.setText("SELECT * FROM Address $P!{OrderByClause}");
    jasperDesign.setQuery(query);

    //Fields
    JRDesignField field = new JRDesignField();
    field.setName("Id");
    field.setValueClass(java.lang.Integer.class);
    jasperDesign.addField(field);

    field = new JRDesignField();
    field.setName("FirstName");
    field.setValueClass(java.lang.String.class);
    jasperDesign.addField(field);

    field = new JRDesignField();
    field.setName("LastName");
    field.setValueClass(java.lang.String.class);
    jasperDesign.addField(field);

    field = new JRDesignField();
    field.setName("Street");
    field.setValueClass(java.lang.String.class);
    jasperDesign.addField(field);

    field = new JRDesignField();
    field.setName("City");
    field.setValueClass(java.lang.String.class);
    jasperDesign.addField(field);

    //Variables
    JRDesignVariable variable = new JRDesignVariable();
    variable.setName("CityNumber");
    variable.setValueClass(java.lang.Integer.class);
    variable.setResetType(ResetTypeEnum.GROUP);
    JRDesignGroup group = new JRDesignGroup();
    group.setName("CityGroup");
    variable.setResetGroup(group);
    variable.setCalculation(CalculationEnum.SYSTEM);
    JRDesignExpression expression = new JRDesignExpression();
    expression.setValueClass(java.lang.Integer.class);
    expression.setText("($V{CityNumber} != null)?(new Integer($V{CityNumber}.intValue() + 1)):(new Integer(1))");
    variable.setInitialValueExpression(expression);
    jasperDesign.addVariable(variable);

    variable = new JRDesignVariable();
    variable.setName("AllCities");
    variable.setValueClass(java.lang.String.class);
    variable.setResetType(ResetTypeEnum.REPORT);
    variable.setCalculation(CalculationEnum.SYSTEM);
    jasperDesign.addVariable(variable);

    //Groups
    group.setMinHeightToStartNewPage(60);
    expression = new JRDesignExpression();
    expression.setValueClass(java.lang.String.class);
    expression.setText("$F{City}");
    group.setExpression(expression);

    JRDesignBand band = new JRDesignBand();
    band.setHeight(20);
    JRDesignTextField textField = new JRDesignTextField();
    textField.setX(0);
    textField.setY(4);
    textField.setWidth(515);
    textField.setHeight(15);
    textField.setBackcolor(new Color(0xC0, 0xC0, 0xC0));
    textField.setMode(ModeEnum.OPAQUE);
    textField.setHorizontalAlignment(HorizontalAlignEnum.LEFT);
    textField.setStyle(boldStyle);
    expression = new JRDesignExpression();
    expression.setValueClass(java.lang.String.class);
    expression.setText("\"  \" + String.valueOf($V{CityNumber}) + \". \" + String.valueOf($F{City})");
    textField.setExpression(expression);
    band.addElement(textField);
    JRDesignLine line = new JRDesignLine();
    line.setX(0);
    line.setY(19);
    line.setWidth(515);
    line.setHeight(0);
    band.addElement(line);
    ((JRDesignSection)group.getGroupHeaderSection()).addBand(band);

    band = new JRDesignBand();
    band.setHeight(20);
    line = new JRDesignLine();
    line.setX(0);
    line.setY(-1);
    line.setWidth(515);
    line.setHeight(0);
    band.addElement(line);
    JRDesignStaticText staticText = new JRDesignStaticText();
    staticText.setX(400);
    staticText.setY(0);
    staticText.setWidth(60);
    staticText.setHeight(15);
    staticText.setHorizontalAlignment(HorizontalAlignEnum.RIGHT);
    staticText.setStyle(boldStyle);
    staticText.setText("Count : ");
    band.addElement(staticText);
    textField = new JRDesignTextField();
    textField.setX(460);
    textField.setY(0);
    textField.setWidth(30);
    textField.setHeight(15);
    textField.setHorizontalAlignment(HorizontalAlignEnum.RIGHT);
    textField.setStyle(boldStyle);
    expression = new JRDesignExpression();
    expression.setValueClass(java.lang.Integer.class);
    expression.setText("$V{CityGroup_COUNT}");
    textField.setExpression(expression);
    band.addElement(textField);
    ((JRDesignSection)group.getGroupFooterSection()).addBand(band);

    jasperDesign.addGroup(group);

    //Title
    band = new JRDesignBand();
    band.setHeight(50);
    line = new JRDesignLine();
    line.setX(0);
    line.setY(0);
    line.setWidth(515);
    line.setHeight(0);
    band.addElement(line);
    textField = new JRDesignTextField();
    textField.setBlankWhenNull(true);
    textField.setX(0);
    textField.setY(10);
    textField.setWidth(515);
    textField.setHeight(30);
    textField.setHorizontalAlignment(HorizontalAlignEnum.CENTER);
    textField.setStyle(normalStyle);
    textField.setFontSize(22);
    expression = new JRDesignExpression();
    expression.setValueClass(java.lang.String.class);
    expression.setText("$P{ReportTitle}");
    textField.setExpression(expression);
    band.addElement(textField);
    jasperDesign.setTitle(band);
    
    //Page header
    band = new JRDesignBand();
    band.setHeight(20);
    JRDesignFrame frame = new JRDesignFrame();
    frame.setX(0);
    frame.setY(5);
    frame.setWidth(515);
    frame.setHeight(15);
    frame.setForecolor(new Color(0x33, 0x33, 0x33));
    frame.setBackcolor(new Color(0x33, 0x33, 0x33));
    frame.setMode(ModeEnum.OPAQUE);
    band.addElement(frame);
    staticText = new JRDesignStaticText();
    staticText.setX(0);
    staticText.setY(0);
    staticText.setWidth(55);
    staticText.setHeight(15);
    staticText.setForecolor(Color.white);
    staticText.setBackcolor(new Color(0x33, 0x33, 0x33));
    staticText.setMode(ModeEnum.OPAQUE);
    staticText.setHorizontalAlignment(HorizontalAlignEnum.CENTER);
    staticText.setStyle(boldStyle);
    staticText.setText("ID");
    frame.addElement(staticText);
    staticText = new JRDesignStaticText();
    staticText.setX(55);
    staticText.setY(0);
    staticText.setWidth(205);
    staticText.setHeight(15);
    staticText.setForecolor(Color.white);
    staticText.setBackcolor(new Color(0x33, 0x33, 0x33));
    staticText.setMode(ModeEnum.OPAQUE);
    staticText.setStyle(boldStyle);
    staticText.setText("Name");
    frame.addElement(staticText);
    staticText = new JRDesignStaticText();
    staticText.setX(260);
    staticText.setY(0);
    staticText.setWidth(255);
    staticText.setHeight(15);
    staticText.setForecolor(Color.white);
    staticText.setBackcolor(new Color(0x33, 0x33, 0x33));
    staticText.setMode(ModeEnum.OPAQUE);
    staticText.setStyle(boldStyle);
    staticText.setText("Street");
    frame.addElement(staticText);
    jasperDesign.setPageHeader(band);

    //Column header
    band = new JRDesignBand();
    jasperDesign.setColumnHeader(band);

    //Detail
    band = new JRDesignBand();
    band.setHeight(20);
    textField = new JRDesignTextField();
    textField.setX(0);
    textField.setY(4);
    textField.setWidth(50);
    textField.setHeight(15);
    textField.setHorizontalAlignment(HorizontalAlignEnum.RIGHT);
    textField.setStyle(normalStyle);
    expression = new JRDesignExpression();
    expression.setValueClass(java.lang.Integer.class);
    expression.setText("$F{Id}");
    textField.setExpression(expression);
    band.addElement(textField);
    textField = new JRDesignTextField();
    textField.setStretchWithOverflow(true);
    textField.setX(55);
    textField.setY(4);
    textField.setWidth(200);
    textField.setHeight(15);
    textField.setPositionType(PositionTypeEnum.FLOAT);
    textField.setStyle(normalStyle);
    expression = new JRDesignExpression();
    expression.setValueClass(java.lang.String.class);
    expression.setText("$F{FirstName} + \" \" + $F{LastName}");
    textField.setExpression(expression);
    band.addElement(textField);
    textField = new JRDesignTextField();
    textField.setStretchWithOverflow(true);
    textField.setX(260);
    textField.setY(4);
    textField.setWidth(255);
    textField.setHeight(15);
    textField.setPositionType(PositionTypeEnum.FLOAT);
    textField.setStyle(normalStyle);
    expression = new JRDesignExpression();
    expression.setValueClass(java.lang.String.class);
    expression.setText("$F{Street}");
    textField.setExpression(expression);
    band.addElement(textField);
    line = new JRDesignLine();
    line.setX(0);
    line.setY(19);
    line.setWidth(515);
    line.setHeight(0);
    line.setForecolor(new Color(0x80, 0x80, 0x80));
    line.setPositionType(PositionTypeEnum.FLOAT);
    band.addElement(line);
    ((JRDesignSection)jasperDesign.getDetailSection()).addBand(band);

    //Column footer
    band = new JRDesignBand();
    jasperDesign.setColumnFooter(band);

    //Page footer
    band = new JRDesignBand();
    jasperDesign.setPageFooter(band);

    //Summary
    band = new JRDesignBand();
    jasperDesign.setSummary(band);
    
    return jasperDesign;
  }
}