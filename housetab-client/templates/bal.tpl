<apply template="base">

  <div class="row">
    <div class="col-xs-12">
      <form class="form-inline form-padded">
        <div class="form-group">
          <select name="mode" class="form-control">
            <option selected="selected" value="bal">bal</option>
            <option value="reg">reg</option>
          </select>
        </div>
        <div class="form-group">
          <input type="text" class="form-control" name="filter" value="${filter}">
        </div>
        <div class="form-group">
          <input type="text" name="start_date" class="form-control date" value="${start_date}">
        </div>
        to
        <div class="form-group">
          <input type="text" name="end_date" class="form-control date" value="${end_date}">
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
            <td><balance/></td>
            <td><account/></td>
          </tr>
        </entries>
      </table>
    </div>
  </div>



</apply>
