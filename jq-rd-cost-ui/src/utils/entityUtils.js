export function cascader(obj, fields=[]) {
    fields.forEach(field=>{
        if (obj[field]!=null && obj[field] instanceof Array) {
            obj[field]=obj[field].join(',')
        }
    })
}


export function deCascader(obj, fields=[]) {
    fields.forEach(field => {
        if (obj[field] != null ) { 
            obj[field]=obj[field].split(',')
        }
    })
}