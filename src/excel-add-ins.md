---
eleventyNavigation:
  key: Excel Add-ins
layout: topic-layout.njk
---

See <https://docs.microsoft.com/en-us/office/dev/add-ins/overview/office-add-ins-fundamentals>.
Also see <https://docs.microsoft.com/en-us/office/dev/add-ins/develop/develop-add-ins-vscode>.

To install a tool that generates the initial code for an add-in:

```bash
npm install -g yo generator-office
```

To create an add-in for Excel that is implemented with React and TypeScript:

```bash
yo office --projectType react --name "my-excel-add-in" --host excel --ts true
```

cd my-excel-add-in
npm i -D nan
npm audit fix
npm start
You will be prompted for a password.
Don't enter one. Instead, press ctrl-c to exit.

Open the "Keychain Access" app in Applications/utilities.
Open the Finder.
Open the ~/.office-addin-dev-certs directory.
Drag the file `ca.crt` onto "Keychain Access".

npm start

This time it should not prompt for a password.
It will open Excel.
Press "Show Taskpane" on the far right.
Press "Show Certificate".
Open the "Trust" section.
In the "When using this certificate" dropdown, select "Always Trust".
Press "Continue".

Press "Restart".

If you modify `manifest.xml`, enter `npm run validate` to validate it.

Saving changes should trigger a hot reload inside Excel.
You can also press "> Run" in the Taskpane to refresh the React app.

From a Terminal window enter:

```text
defaults write com.microsoft.Excel OfficeWebAddinDeveloperExtras -bool true
```

To see the DevTools Console, right-click anywhere in the running React app
and select "Inspect Element" to open the Safari Web Inspector.
Then click the "Console" tab.
Output from `console.log` calls will appear here.
