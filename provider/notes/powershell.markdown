---
title: Powershell
published: 2016-04-27 12:00:00
updated: 2017-10-12 09:05:45
subtitle: Quick notes on common powershell commands
tags: notes, windows, powershell
---

<aside class="sidenote">
Currently powershell highlighting is not working. Support for powershell hughlighting was added in [this commit](https://github.com/jgm/skylighting/commit/e27223aa511f572bf1efbf7fb1a58d41d2c75f15).
Once `skylighting>=0.4.1` make it to a [LTS snapshot](https://www.stackage.org/package/skylighting) highlighting will begin to work.
</aside>

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
