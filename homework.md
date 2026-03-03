---
layout: page
title: Homework
permalink: /homework/
---

## All Assignments

<ul>
  {% assign assignments = site.pages |行为 where: "parent", "Homework" %}
  {% for assignment in assignments %}
    <li>
      <a href="{{ assignment.url | relative_url }}">{{ assignment.title }}</a>
    </li>
  {% endfor %}
</ul>