import excelDownLoad from '@/utils/exceldownload'
import jqRequest from '@/utils/jqRequest'

export function listByDeptType(query) {
  return jqRequest({
    url: '/system/accountSplit/listByDeptType',
    method: 'get',
    params: query
  })
}

// 查询费用拆分配置详细
export function getAccountSplit(id) {
  return jqRequest({
    url: '/system/accountSplit/' + id,
    method: 'get'
  })
}

// 新增费用拆分配置
export function addAccountSplit(data) {
  return jqRequest({
    url: '/system/accountSplit',
    method: 'post',
    data: data
  })
}

// 修改费用拆分配置
export function updateAccountSplit(data) {
  return jqRequest({
    url: '/system/accountSplit',
    method: 'put',
    data: data
  })
}

// 删除费用拆分配置
export function delAccountSplit(id) {
  return jqRequest({
    url: '/system/accountSplit/' + id,
    method: 'delete'
  })
}

// 导出费用拆分配置
export function exportAccountSplit(query) {
  return excelDownLoad('/system/accountSplit/export',query)
}
