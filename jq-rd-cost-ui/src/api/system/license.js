import request from '@/utils/jqRequest'



// 查询授权信息
export function license() {
  return request({
    url: '/license/getServerAndLicense',
    method: 'get',
  })
}

// 上传许可
export function uploadLicense(file) {
  return request({
    url: '/license/upload',
    method: 'post',
    data: file
  })
}
