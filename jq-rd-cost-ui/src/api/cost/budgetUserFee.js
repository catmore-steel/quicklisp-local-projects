import request from '@/utils/request'
import excelDownLoad from '@/utils/exceldownload'

// 查询人员费用拟定;详细
export function   getBudgetUserFee(id) {
  return request({
    url: '/cost/budgetUserFee/' + id,
    method: 'get'
  })
}

// 新增人员费用拟定;
export function addBudgetUserFee(data) {
  return request({
    url: '/cost/budgetUserFee',
    method: 'post',
    data: data
  })
}

// 修改人员费用拟定;
export function updateBudgetUserFee(data) {
  return request({
    url: '/cost/budgetUserFee',
    method: 'put',
    data: data
  })
}

// 删除人员费用拟定;
export function delBudgetUserFee(id) {
  return request({
    url: '/cost/budgetUserFee/' + id,
    method: 'delete'
  })
}

// 导出人员费用拟定;
export function exportBudgetUserFee(query) {
  return excelDownLoad('/cost/budgetUserFee/export',query)
}

export function getBaseUserInfo(id) {
  return request({
    url: '/cost/budgetUserFee/userInfo/' + id,
    method: 'get'
  })
}

export function updateByUserInfo(data) {
  return request({
    url: '/cost/budgetUserFee/updateByUserInfo',
    method: 'post',
    data: data
  })
}

export function getStatistics(query) {
  return request({
    url: '/cost/budgetUserFee/statistics',
    method: 'get',
    params: query
  })
}

export function checkDraft(data) {
  return request({
    url: '/cost/budgetUserFee/checkDraft',
    method: 'post',
    data: data
  })
}
