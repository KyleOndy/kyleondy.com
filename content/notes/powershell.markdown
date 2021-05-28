---
title: Powershell
updated: 2018-05-03T20:32:06Z
subtitle: Quick notes on common powershell commands
tags: notes, windows, powershell
---

Reading a file in realtime, just like `tail -f` with linux

~~~{.powershell}
Get-Content -Path \\Path\To\File -Wait
~~~

----

Copy groups from one user to another

~~~{.powershell}
Get-Aduser -Identity UserToCopyGroupsFrom -Properties MemberOf |
    Select -ExpandProperty MemberOf
    Add-ADGroupMember -Members UseBeingAddedToGroups -PassThru |
    Select SamAccountName
~~~

Enable remote desktop (assuming WinRM is enabled)

~~~{.powershell}
Invoke-Command -ComputerName PC -Credential (Get-Credential AdminUser) -ScriptBlock {
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server'-name "fDenyTSConnections" -Value 0
    Set-ItemProperty -Path 'HKLM:\System\CurrentControlSet\Control\Terminal Server\WinStations\RDP-Tcp' -name "UserAuthentication" -Value 1
}
~~~
