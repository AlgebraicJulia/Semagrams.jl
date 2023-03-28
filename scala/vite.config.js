import { resolve } from 'path'
import { defineConfig } from 'vite'

export default defineConfig({
  build: {
    rollupOptions: {
      input: {
        main: resolve(__dirname, 'index.html'),
        dwd: resolve(__dirname, 'apps/dwd/index.html'),
        simplepetri: resolve(__dirname, 'apps/simplepetri/index.html'),
      }
    }
  }
})
