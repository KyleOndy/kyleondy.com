---
title: WinRM
updated: 2018-11-30T18:25:28Z
subtitle: Checking WinRM
tags: notes, winrm, windows
---

# Checks WinRM service is listening

Port `5985` is default, `5986` for `https`

~~~{.bash}
curl \
  http://192.168.0.10:5985/wsman \
  --header "Content-Type: application/soap+xml;charset=UTF-8" \
  --data '&lt;s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:wsmid="http://schemas.dmtf.org/wbem/wsman/identity/1/wsmanidentity.xsd"&gt;&lt;s:Header/&gt;&lt;s:Body&gt;&lt;wsmid:Identify/&gt;&lt;/s:Body&gt;&lt;/s:Envelope&gt;'

# basic auth
--basic -u user:password
~~~

Oneliner for copy and paste

~~~{.bash}
curl http://192.168.0.10:5985/wsman --header "Content-Type: application/soap+xml;charset=UTF-8" --data '&lt;s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:wsmid="http://schemas.dmtf.org/wbem/wsman/identity/1/wsmanidentity.xsd"&gt;&lt;s:Header/&gt;&lt;s:Body&gt;&lt;wsmid:Identify/&gt;&lt;/s:Body&gt;&lt;/s:Envelope&gt;'
~~~
