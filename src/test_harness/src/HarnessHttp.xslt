<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="TopConfig">
    <HTML>
      <HEAD>
        <TITLE>
          <xsl:value-of select="TopFname"/>
        </TITLE>
      </HEAD>
      <BODY>
        <xsl:apply-templates/>
      </BODY>
    </HTML>
  </xsl:template>

  <xsl:template match="ProbDescString">
    <P>
      <xsl:value-of select="PDS"/>
      <TABLE BORDER="2">
        <TR>
          <TD>X</TD>
          <TD>Y</TD>
          <TD>Z</TD>
          <TD/>
          <TD>X</TD>
          <TD>Y</TD>
          <TD>Z</TD>
          <TD/>
          <TD/>
          <TD>X</TD>
          <TD>Y</TD>
          <TD>Z</TD>
          <TD/>
          <TD>X</TD>
          <TD>Y</TD>
          <TD>Z</TD>
        </TR>
        <xsl:apply-templates/>
      </TABLE>
    </P>
  </xsl:template>

  <xsl:template match="DistGridFile/Row">
    <TR>
      <TD>
        <xsl:value-of select="SourceDistSize1"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceDistSize2"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceDistSize3"/>
      </TD>
      <TD/>
      <TD>
        <xsl:value-of select="DestDistSize1"/>
      </TD>
      <TD>
        <xsl:value-of select="DestDistSize2"/>
      </TD>
      <TD>
        <xsl:value-of select="DestDistSize3"/>
      </TD>
      <TD/>
      <TD/>
      <TD>
        <xsl:value-of select="SourceGridSize1"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridSize2"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridSize3"/>
      </TD>
      <TD/>
      <TD>
        <xsl:value-of select="DestGridSize1"/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridSize2"/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridSize3"/>
      </TD>
    </TR>

    <TR>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD>
        <xsl:value-of select="SourceGridCoordLow1"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridCoordLow2"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridCoordLow3"/>
      </TD>
      <TD/>
      <TD>
        <xsl:value-of select="DestGridCoordLow1"/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridCoordLow2"/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridCoordLow3"/>
      </TD>
    </TR>
    
    <TR>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD>
        <xsl:value-of select="SourceGridCoordHigh1"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridCoordHigh2"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridCoordHigh3"/>
      </TD>
      <TD/>
      <TD>
        <xsl:value-of select="DestGridCoordHigh1"/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridCoordHigh2"/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridCoordHigh3"/>
      </TD>
    </TR>

    <TR>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD/>
      <TD>
        <xsl:value-of select="SourceGridType1"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridType2"/>
      </TD>
      <TD>
        <xsl:value-of select="SourceGridType3"/>
      </TD>
      <TD/>
      <TD>
        <xsl:value-of select="DestGridType1"/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridType2"/>
      </TD>
      <TD>
        <xsl:value-of select="DestGridType3"/>
      </TD>
    </TR>
  </xsl:template>

  <xsl:template match="text()|@*"></xsl:template>
</xsl:stylesheet>
