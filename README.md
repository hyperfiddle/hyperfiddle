# Hyperfiddle starter app

For production support teams at operationally heavy businesses, Hyperfiddle is a **programmable object navigator** that reaches all your at-work enterprise objects in prod.

Stop treating your ops teams badly due to eng resource constraints, as the beating heart of the business they should be your primary customer. Deliver **premium quality internal frontends faster**, without needing specialist frontend framework expertise.

![Demo video](./docs/20250617_entity_browser.mp4)

## Features

* prod-ready, built on standard web technologies
* enterprise objects reachable through direct classpath linking
* streamlined navigator UX with table-pickers and powerful grids
* scalable UI infrastructure (powered by [Electric Clojure](https://github.com/hyperfiddle/electric))
* programmable framework, not just a static developer tool
* programmable queries, user controls all queries (direct classpath linking)
* programmable grids, forms, hyperlinks and routing through declarative hypermedia DSL
* full progressive enhancement with web technologies (HTML, CSS, frontend, backend)
* same security model as production web apps
* **NO REST APIs.** Never write an internal tool REST API ever again! (State distribution powered by [Electric Clojure and the Electric protocol](https://github.com/hyperfiddle/electric))

## Direct classpath linking to integrate any data source or native object

* **Not just Datomic:** navigate SQL or any backend data source via classpath function calls
* **Not just databases:** navigate clojure namespaces, file system, internal service state – literally any object
* **Not just Clojure:** call Java functions via classpath linking
* **Not just "data":** navigate **actual Java objects** via method call navigation (!)

**FAQ: What's the difference between an object navigator and a data browser?** Hyperfiddle is about objects, not data. Objects come with functions and methods, they aren't serializable, you navigate hyperlink graph structures rather than browse freeform nested structures, and you want to do this without impedance, using the actual native object datatype, not derived projections or data mappers. Objects are what the world is made of. And Hyperfiddle is how you reach them.

## Example app: Datomic prod support tool

(Hyperfiddle does NOT depend on Datomic. This Datomic example is a *userland* Hyperfiddle app!)

[![20250627_datomic_entity_browser.png](./docs/20250627_datomic_entity_browser.png)](./docs/20250627_datomic_entity_browser.png)

Datomic support app features:
* entity navigation, reverse attributes link
* query diagnostics (io-context etc)
* monitor and kill slow queries from very large databases
* classpath connected for custom queries (direct classpath linking to any function)
* fluid virtual scroll over 50k record collections
* built-in schema explorer with attribute counts
* filtering and sort
* column selection and inference
* derived fields and attributes
* schema browser with attribute counts
* entity tooltips on all IDs
* entity history link* Business-level database explorer
* easy to integrate ring middleware - embed in your at-work httpkit or jetty services
* enterprise SSO (contact us)

[![20250627_datomic_schema_app.png](./docs/20250627_datomic_schema_app.png)](./docs/20250627_datomic_schema_app.png)

## Example app: jGit history search GUI

[![2024_jgit_history_gui.png](./docs/2024_jgit_history_gui.png)](./docs/2024_jgit_history_gui.png)

## More live examples

* [jGit repo explorer]()
* [clojure namespace and var directory](https://electric.hyperfiddle.net/dustingetz.object-browser-demo3!ObjectBrowserDemo3/(dustingetz.object-browser-demo3!clojure-all-ns))
* SQL browser (todo host demo)
* [jvm process thread inspector](https://electric.hyperfiddle.net/dustingetz.object-browser-demo3!ObjectBrowserDemo3/(dustingetz.object-browser-demo3!thread-mx))
* [java class inspector](https://electric.hyperfiddle.net/dustingetz.object-browser-demo3!ObjectBrowserDemo3/(dustingetz.object-browser-demo3!class-view,java.lang.management.ThreadMXBean))
* [file/folder explorer](https://electric.hyperfiddle.net/dustingetz.object-browser-demo3!ObjectBrowserDemo3/(clojure.java.io!file,'.!'))
* jar file viewer

## a foundation for next-gen enterprise apps

Use this foundation to build scalable, enterprise-class frontends that are deeply customizable, robust and secure.

[![2024_hyperfiddle-crud-spreadsheet-explainer-sub.png](./docs/2024_hyperfiddle-crud-spreadsheet-explainer-sub.png)](./docs/2024_hyperfiddle-crud-spreadsheet-explainer-sub.png)

* (coming soon) hypermedia DSL: links, forms, buttons
* (coming soon) editable enterprise datagrids
* (coming soon) enterprise forms, pickers, wizards
* (coming soon) CQRS command/query architecture
* (coming soon) microservice classpath connectors
* (coming soon) audit and log all server effects and classpath interop
* (coming soon) enterprise security middleware
* (coming soon) Python classpaths

Use cases: enterprise workbench apps, customer support tools, microservice internal state observability and debugging tools, internal control plane apps, interactive dashboards.

## Where are we going with this

"Hyper" means interconnected. "Fiddle" means play. Our vision for Hyperfiddle is to develop an **end-user hypermedia programming environment** that reaches all of your at-work cloud objects without writing network glue code. Our mission is to **collapse to zero** the cost of business process frontend development, for a huge range of apps from enterprise applications to throwaway tools.

> "Civilization advances by extending the number of operations we can perform without thinking about them.”

**FAQ: Does it have AI?** Yes, we are starting to experiment with agents and it's going to be every bit as amazing as you imagine.

## Project goals

Hyperfiddle is an ongoing, decade-long R&D investment in bringing to market a new class of foundation GUI infrastructure for business frontends.

Technical goals:
* identify and label the common structure shared between spreadsheets and CRUD apps
* in a credible, enterprise-compatible way that scales to more sophisticated apps, not less
* leverage this structure as the foundation for or substrate of a next-gen application framework or engine (think Unity for enterprise apps)
* a foundation for end user programming as a higher order, creative medium
* zero-code data connectivity via the Electric protocol (c.f. Netscape and HTTP -- Netscape is the original declarative IO runtime for HTML apps)
* **never write a REST integration ever again**

Economic goals:
* find and develop a market, economic model, and global at-scale distribution strategy which is directly and immediately aligned with investing the proceeds into foundational programming abstractions, so that we can all benefit from better software, and build it less painfully

> "If a system is to serve the creative spirit, it must be entirely comprehensible to a single individual. Human potential manifests itself in individuals." — Dan Ingalls, Design Principles behind Smalltalk

## Social media

* https://x.com/dustingetz

## License
* free for individual use on local dev machines, mandatory runtime login (we are a business)
* using in prod requires a license, contact us.
* still working out the details

## Getting started

```
git clone git@gitlab.com:hyperfiddle/hyperfiddle-starter-app.git
cd hyperfiddle-starter-app

# Install demo data
java -version              # we use openjdk version "23.0.2"
./datomic_fixtures.sh      # get Datomic (free) and example data
./run_datomic.sh

# Run demo app. You’ll be asked to authenticate.
# First via Clojure CLI to see it working. https://clojure.org/guides/install_clojure
clj -X:dev dev/-main :datomic-uri '"datomic:dev://localhost:4334/mbrainz-1968-1973"'

# Now jack in to REPL, :dev alias:
user=> (dev/-main {:datomic-uri "datomic:dev://localhost:4334/mbrainz-1968-1973"})
```

# Program your business, without drowning in complexity

<p align="center">
  <img width="500" src="./docs/2024_logo-hyperfiddle-crud-spreadsheet-transparent.svg">
</p>