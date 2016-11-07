{%- extends 'slides_reveal.tpl' -%}

{% block input_group -%}
<div class="input_hidden">
{{ super() }}
</div>
{% endblock input_group %}

{%- block header -%}
{{ super() }}

<style type="text/css">
.input_hidden {
  display: none;
}
</style>
{%- endblock header -%}