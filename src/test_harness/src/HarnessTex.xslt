<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="TopConfig">
    \\documentclass{report}
    \\begin{document}
    \\section{<xsl:value-of select="TopFname"/>}
    \\label{sec:<xsl:value-of select="TopFname"/>}
        <xsl:apply-templates/>
    \\linebreak
    End of Report
    \\end{document}
  </xsl:template>

  <xsl:template match="ProbDescString">
    \\linebreak
      <xsl:value-of select="PDS"/>
      <xsl:apply-templates/>
  </xsl:template>

  <xsl:template match="DistGridFile/Row">
    \\linebreak
        <xsl:value-of select="SourceDistSize1"/>
  </xsl:template>

  <xsl:template match="text()|@*"></xsl:template>
</xsl:stylesheet>
