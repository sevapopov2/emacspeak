<?xml version="1.0"?>
<!--$Id: auto-rss-discover.xsl,v 19.0 2003/11/22 19:06:53 raman Exp $-->

<!--
Author: T. V. Raman <raman@cs.cornell.edu>
Copyright: (C) T. V. Raman, 2001 - 2002,   All Rights Reserved.
License: GPL
Description: Display all RSS links
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
  <xsl:output method="text" indent="yes" encoding="iso8859-1"/>
  
  <xsl:template match="/">
    <xsl:for-each select="//link[@type='application/rss+xml']">
      <xsl:value-of select="@href"/><xsl:text>
    </xsl:text></xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
<!--
Local Variables:
mode: xae
sgml-indent-step: 2
sgml-indent-data: t
sgml-set-face: nil
sgml-insert-missing-element-comment: nil
folded-file: t
End:
-->
