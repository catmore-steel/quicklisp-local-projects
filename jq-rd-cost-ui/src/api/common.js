// 通用上传请求
import request from '@/utils/jqRequest'

// 上传富文本图片附件
export function uploadEditorImage(data) {
  return request({
    url: '/common/uploadEditorImage',
    method: 'post',
    data: data
  })
}


// 重命名附件名称
export function resetName(data) {
  return request({
    url: '/common/attch/resetName',
    method: 'put',
    params: data
  })
}
