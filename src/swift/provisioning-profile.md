---
eleventyNavigation:
  key: Provisioning Profiles
  parent: Swift
layout: topic-layout.njk
---

## Overview

A "provisioning profile" is required to enable app installations
for both development and distribution in the App Store.

## Steps to Create

- browse https://developer.apple.com/account
- click the box labelled "Certificates, Indentifiers & Profiles"
- click "Profiles" in the left nav
- click the "Generate a profile" button
- select the kind of development such as "iOS App Development"
- click the "Continue" button
- select the id of an app that has already been created
- click the "Continue" button
- select a certificate
- click the "Continue" button
- select the devices where it will be used
- click the "Continue" button
- enter a name for the provisioning profile
- click the "Generate" button
- click the "Download" button

## Steps to Use

- open the project in Xcode
- select the top item in the Navigator
- select the first target in the left nav
- select the "Signing & Capabilities" tab
- click the "Provisioning Profile" dropdown
- select "Import Profile..."
- select the downloaded file
- click the "Open" button
