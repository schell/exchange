<apply template="index">
  <bind tag="main">
    <form method="POST">
      <fieldset>
        <legend>New Page</legend>
        <label>Page Path:</label>
        <input name="templatePath" type="text" placeholder="path/to/page" />
        <br />
        <label>Page Source:</label>
        <textarea name="templateSrc" rows="5" style="width:100%"></textarea>
        <span class="help-block">Example block-level help text here.</span>
        <!--
        <label class="checkbox">
          <input type="checkbox"> Check me out
        </label>
        -->
        <button type="submit" class="btn">Submit</button>
      </fieldset>
    </form>
  </bind>
</apply>
