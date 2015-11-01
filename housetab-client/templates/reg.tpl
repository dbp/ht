<apply template="base">

  <div class="row">
    <div class="col-xs-12">
      <form class="form-inline form-padded">
        <div class="form-group">
          <select name="mode" class="form-control">
            <option value="bal">bal</option>
            <option selected="selected" value="reg">reg</option>
          </select>
        </div>
        <div class="form-group">
          <input type="text" class="form-control" id="filter" value="expenses:groceries">
        </div>
        <div class="form-group">
          <input type="text" name="start_date" class="form-control date">
        </div>
        to
        <div class="form-group">
          <input type="text" name="end_date" class="form-control date">
        </div>
        <button type="submit" class="btn btn-default">Go</button>
      </form>
    </div>
  </div>

  <div class="row">
    <div class="col-xs-12">
      <table class="table table-condensed table-bordered table-striped">
        <entries>
          <tr>
            <td><date/></td>
            <td><desc/></td>
            <td><account/></td>
            <td><amount/></td>
            <td><balance/></td>
          </tr>
        </entries>
      </table>
    </div>
  </div>



</apply>
