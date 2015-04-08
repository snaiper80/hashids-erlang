

# Module hashids #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-hashids_context">hashids_context()</a> ###



<pre><code>
hashids_context() = #hashids_context{salt = list(), min_length = non_neg_integer(), alphabet = list(), seperators = list(), guards = list()}
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#alphabet-1">alphabet/1</a></td><td>returns adjusted custom alphabet from context.</td></tr><tr><td valign="top"><a href="#decode-2">decode/2</a></td><td>decode hash string.</td></tr><tr><td valign="top"><a href="#encode-2">encode/2</a></td><td>encode numbers.</td></tr><tr><td valign="top"><a href="#min_hash_length-1">min_hash_length/1</a></td><td>returns minimum hash length from context.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>make a new hashids context (convenient function).</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>make a new hashids context.</td></tr><tr><td valign="top"><a href="#salt-1">salt/1</a></td><td>returns salt from context.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="alphabet-1"></a>

### alphabet/1 ###


<pre><code>
alphabet(Context::<a href="#type-hashids_context">hashids_context()</a>) -&gt; string()
</code></pre>
<br />

returns adjusted custom alphabet from context
<a name="decode-2"></a>

### decode/2 ###


<pre><code>
decode(Context::<a href="#type-hashids_context">hashids_context()</a>, HashStr::string()) -&gt; [integer(), ...]
</code></pre>
<br />

decode hash string
<a name="encode-2"></a>

### encode/2 ###


<pre><code>
encode(Context::<a href="#type-hashids_context">hashids_context()</a>, N::integer() | [integer(), ...]) -&gt; string()
</code></pre>
<br />

encode numbers
<a name="min_hash_length-1"></a>

### min_hash_length/1 ###


<pre><code>
min_hash_length(Context::<a href="#type-hashids_context">hashids_context()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

returns minimum hash length from context
<a name="new-0"></a>

### new/0 ###


<pre><code>
new() -&gt; <a href="#type-hashids_context">hashids_context()</a>
</code></pre>
<br />

make a new hashids context (convenient function)
<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Opts::[] | [{salt | default_alphabet | min_hash_length, any()}]) -&gt; <a href="#type-hashids_context">hashids_context()</a>
</code></pre>
<br />

make a new hashids context
<a name="salt-1"></a>

### salt/1 ###


<pre><code>
salt(Context::<a href="#type-hashids_context">hashids_context()</a>) -&gt; string()
</code></pre>
<br />

returns salt from context
