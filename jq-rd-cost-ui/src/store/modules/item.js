const item = {
  state: {
    companyId: null,
    itemNo: null,

  }, mutations: {
    SET_COMPANY_ID: (state, companyId) => {
      state.companyId = companyId
    },
    SET_ITEM_NO: (state, itemNo) => {
      state.itemNo = itemNo
    },
  }, actions: {}
}
export default item
