<apply template="base">


  <div class="row">
    <div class="col-xs-12">

      <form>
        <div class="form-group">
          <label for="cost">Cost</label>

          <div class="input-group">
            <span class="input-group-addon">$</span>
            <input type="text" class="form-control" name="cost" >
          </div>
        </div>
        <div class="form-group">
          <label for="desc">Description</label>
          <input type="text" name="desc" class="form-control" id="desc" placeholder="Purchase description...">
        </div>
        <div class="form-group">
          <label for="date">Date</label>
          <input type="text" name="date" class="form-control date">
        </div>

        <div class="form-group">
          <label for="for">Account From</label>
          <input type="text" id="from" name="from" class="form-control" placeholder="assets:...">
        </div>

        <div class="form-group">
          <label for="for">Account To</label>
          <input type="text" id="to" name="to" class="form-control" placeholder="expenses:...">
        </div>

        <button type="submit" class="btn btn-default">Add</button>
      </form>

    </div>
  </div>
</apply>
