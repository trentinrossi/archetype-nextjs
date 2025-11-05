'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

export default function Home() {
  const router = useRouter();

  const features = [
    {
      title: 'Widgets',
      description: 'Manage and display interactive widgets on your dashboard.',
      icon: (
        <svg className="w-8 h-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z" />
        </svg>
      ),
      path: '/widgets',
      color: 'bg-blue-500',
    },
    {
      title: 'Gadgets',
      description: 'View and manage gadget information, specifications, and availability.',
      icon: (
        <svg className="w-8 h-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" />
        </svg>
      ),
      path: '/gadgets',
      color: 'bg-green-500',
    },
    {
      title: 'Dummy Feature',
      description: 'Manage credit cards, view card details, and monitor card status.',
      icon: (
        <svg className="w-8 h-8" fill="none" viewBox="0 0 24 24" stroke="currentColor">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M3 10h18M7 15h1m4 0h1m-7 4h12a3 3 0 003-3V8a3 3 0 00-3-3H6a3 3 0 00-3 3v8a3 3 0 003 3z" />
        </svg>
      ),
      path: '/dummy-feature',
      color: 'bg-purple-500',
    },
  ];

  const handleNavigate = (path: string) => {
    router.push(path);
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-gray-50 to-gray-100">
      <div className="container mx-auto px-4 py-12">
        {/* Header */}
        <div className="text-center mb-12">
          <h1 className="text-5xl font-bold text-gray-900 mb-4">
            Program name here
          </h1>
          <p className="text-xl text-gray-600 max-w-3xl mx-auto">
            Description about the system capabilities and features offered to users.
          </p>
        </div>

        {/* Features Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8 mb-12">
          {features.map((feature) => (
            <div
              key={feature.path}
              className="bg-white rounded-lg shadow-lg overflow-hidden hover:shadow-xl transition-shadow duration-300"
            >
              <div className={`${feature.color} p-6 text-white`}>
                <div className="flex items-center justify-center mb-4">
                  {feature.icon}
                </div>
                <h2 className="text-2xl font-bold text-center">{feature.title}</h2>
              </div>
              <div className="p-6">
                <p className="text-gray-600 mb-6 text-center min-h-[60px]">
                  {feature.description}
                </p>
                <Button
                  onClick={() => handleNavigate(feature.path)}
                  className="w-full"
                >
                  Go to {feature.title}
                </Button>
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
