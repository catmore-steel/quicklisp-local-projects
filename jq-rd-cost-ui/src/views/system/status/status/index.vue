<template>
  <div>
    <br>
    <!--<el-form ref="form" :model="queryParams" label-width="100px">
      <el-row>
        <el-col :span="8">
          <el-form-item label="材料类型名称" prop="fileTypeName">
            <el-input v-model="queryParams.fileTypeName" class="form-control" placeholder="请输入材料类型名称"/>
          </el-form-item>
        </el-col>
        <el-col :span="12" :offset="1">
          <el-button type="primary" @click="searchStatus">搜 索</el-button>
        </el-col>
      </el-row>
    </el-form>-->

    <el-table :data="fileStatusList">
      <el-table-column type="selection" width="55" align="center"/>
      <el-table-column prop="order" label="材料顺序" width="100px"></el-table-column>
      <el-table-column prop="fileTypeName" label="材料类型名称" width="600px"></el-table-column>
      <el-table-column prop="fileStatus" label="状态">
        <template slot-scope="scope">
          <el-switch v-model="scope.row.fileStatus"
                     inactive-value="2"
                     inactive-text="待签章材料"
                     active-value="3"
                     active-text="最终材料"
                     @change="handleStatusChange(scope.row)"
          ></el-switch>
        </template>
      </el-table-column>
      <el-table-column prop="createBy" label="创建人"></el-table-column>
      <el-table-column prop="createTime" label="创建时间"></el-table-column>
    </el-table>
    <pagination
      v-show="total>0"
      :total="total"
      :page.sync="queryParams.pageNum"
      :limit.sync="queryParams.pageSize"
      @pagination="searchStatus"
    />
    <br>
    <div style="text-align: center">
      <el-button type="primary" @click="submitForm">确 定</el-button>
      <!--<el-button @click="handleClose">取 消</el-button>-->
    </div>
  </div>
</template>

<script>
  import { listFileStatus, saveFileStatus, updateStatus } from '@/api/system/status'

  export default {
    name: 'index',
    data() {
      return {
        // 总条数
        total: 0,
        queryParams: {
          fileTypeName: null,
          pageNum: 1,
          pageSize: 30
        },
        fileStatusList: []
      }
    },
    created() {
      this.searchStatus()
    },
    methods: {
      searchStatus() {
        listFileStatus(this.queryParams).then(res => {
          this.fileStatusList = res.rows
          this.total = res.total
        })
      },
      submitForm() {
        saveFileStatus(this.fileStatusList).then(res => {
          if (res.code === 200) {
            this.msgSuccess(res.msg)
          }
          this.searchStatus()
        })
      },
      handleStatusChange(row) {
        this.$confirm('确认要修改【' + row.fileTypeName + '】的状态吗?', '警告', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(function() {
          return updateStatus(row)
        }).then(() => {
          this.msgSuccess('修改成功！')
          this.searchStatus()
        }).catch(function() {
          row.fileStatus = row.fileStatus === '2' ? '3' : '2'
        })
      }
    }
  }
</script>

<style scoped>

</style>
