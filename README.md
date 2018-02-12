# brandwatchR

An API wrapper package for the [Brandwatch API](https://developers.brandwatch.com/docs/).

If you're like me and you just want to get your hands on lots of lovely mention data, you do that by:

- Authenticating with `bwr_auth()`
- Working out the project id you need with `bwr_get_projects()`
- Working out the id of the query you want results for with `bwr_get_queries()`.

From here, you can retrieve mentions data using `bwr_get_mentions()`, which takes the project ID and query ID as arguments.


## Install

```r
devtools::install_github("Phippsy/brandwatchR")
```

## Authenticate

Only username and password are mandatory. 
You can optionally specify `refresh = TRUE` to force the function to obtain a new token from the API. Otherwise it will read a cached local file if available. 
You can also optionally specify `cache = FALSE` if you prefer not to have your token cached locally. In this case, your token will be stored in a local environment variable and available for the current R session only.

```r
bwr_auth(username = "your@username.com",
         password = "your_password",
         refresh = TRUE,
         cache = TRUE)
```

## Get project info

```r
my_projects <- bwr_projects_get()
head(my_projects)
```

This will display a data frame of project information including project ID, description, creation date and more.

Read the [Brandwatch documentation for more info](https://developers.brandwatch.com/docs/retrieving-projects)

## Get queries for a project

Once you have a project ID, you can find out all the available queries in your environment. Do this using the `bwr_query_get()` function:

```r
# We arbitrarily pull the first available project ID
my_project <- bwr_projects_get()$id[1]

# Get list of queries for this project ID
my_query <- bwr_query_get(project_id = my_project)
```

This will return a data frame showing you the ID, name, created date and similar metadata for the queries in the specified project.

## Get mentions for a query

Finally, the fun stuff - we can now request all mentions for a given query, using a specified date range. We achieve that using the `bwr_mentions_get()` function.

```r
my_mentions <- bwr_mentions_get(
  project_id = my_project,
  query_id = my_query,
  date_range = c("2018-01-01", "2018-02-01"))
```

This will return a data frame containing the mentions for your specified query. Brandwatch returns a *ton* of fields in the API response, so it's likely that you'll want to slim down the resulting data frame by using `dplyr`'s `select()` function or similar.

You can find information about the mention field definitions within [Brandwatch's API documentation](https://developers.brandwatch.com/docs/mention-metadata-field-definitions).


## All functions currently available

#### Authentication

- `bwr_auth()` authenticate

#### Projects

- `bwr_projects_get()`- get a dataframe of projects

#### Queries / mentions

- `bwr_query_get()` - get a dataframe of queries for a project
- `bwr_query_check()` - check the syntax of a query 
- `bwr_query_create()` - upload a new query
- `bwr_query_delete()` - delete a specified query
- `bwr_mentions_get()` - get the mentions for a specified query
- `bwr_mentions_total()` - get the total number of mentions for a specified query or query group.
- `bwr_mentions_topics()` - get the matching topics & topic metadata for a specified query or query group.
- `bwr_mentions_topsites()` - get the top authors for a specified query or query group.
- `bwr_mentions_toptweeters()` - get the top tweeters for a specified query or query group.

##### Query filters
- `bwr_filters_get()` - Get a data frame of all parameters which can be used to filter your query
- `bwr_metrics_get()` - Get a data frame of all metrics which have limited accepted values.

##### Query Groups

- `bwr_querygrp_get()` - get the query groups for a specified project
- `bwr_querygrp_delete()` - delete a specfied query group

#### Tags 

- `bwr_tag_get()` - get the tags for a specified project
- `bwr_tag_create()`  - create a new tag for a specified project
- `bwr_tag_delete()` - delete a tag for a specified project

#### Categories

- `bwr_cat_get()` - get the categories for a specified project
- `bwr_cat_create()`  - create a new category for a specified project

#### Rules

- `bwr_rule_get()` - get a data frame of all rules available within the specified project.
- `bwr_rule_delete()` - delete a rule for a specified project and rule id
