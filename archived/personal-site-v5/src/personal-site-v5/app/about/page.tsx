"use client"

import { motion } from "framer-motion"
import { CyclingPolygon } from "@/components/cycling-polygon"
import Link from "next/link"

export default function About() {
  return (
    <main className="min-h-screen flex items-center justify-center bg-white px-2 py-4">
      <div className="flex items-stretch justify-between w-full max-w-sm sm:max-w-md">
        <div className="flex items-center">
          <Link href="/">
            <CyclingPolygon className="w-12 h-12 text-gray-800" />
          </Link>
        </div>

        <div className="mx-2 flex flex-col justify-center">
          <motion.div
            className="space-y-6"
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.8 }}
          >
            <div className="space-y-4 text-gray-800 font-serif text-sm sm:text-base leading-relaxed text-center">
              <p>
                Gabriel Ong is a second-year undergraduate student reading Computing and Law at Singapore Management
                University.
              </p>
              <p>
                Gabriel works at the Centre for Digital Law based out of Singapore Management University, which
                researches legal technology through computer science.
              </p>
              <p>
                In his free time, Gabriel enjoys writing software that benefits programmers, practising lawyers and law
                students.
              </p>
              <p>
                Gabriel is interested in pursuing the entrepreneurship of legal technology in Singapore later in his
                career.
              </p>
            </div>
          </motion.div>

          <footer className="text-center text-gray-600 font-serif text-xs mt-4">
            <p>© {new Date().getFullYear()} Gabriel Ong / All rights reserved.</p>
          </footer>
        </div>

        <div className="flex items-center">
          <CyclingPolygon className="w-12 h-12 text-gray-800" />
        </div>
      </div>
    </main>
  )
}