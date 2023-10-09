class BoutiqueInventory
  def initialize(items)
    @items = items
  end

  def item_names
    items.each {|item| item[:name]}
  end

  def cheap
    items.select {|item| item[:price] < 30}
  end

  def out_of_stock
    items.select {|item| item[:quantity_by_size]}
  end

  def stock_for_item(name)
    (items.find {|item| item[:name] == name})[[:quantity_by_size]]
  end

  def total_stock
    raise 'Implement the BoutiqueInventory#total_stock method'
  end

  private
  attr_reader :items
end
