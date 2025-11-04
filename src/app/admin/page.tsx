'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { useAuth } from '@/contexts/AuthContext';
import { Button } from '@/components/ui';

export default function AdminMenuPage() {
  const router = useRouter();
  const { user, logout, isAuthenticated, isAdmin } = useAuth();
  const [currentDateTime, setCurrentDateTime] = useState('');

  useEffect(() => {
    if (!isAuthenticated) {
      router.push('/login');
      return;
    }

    if (!isAdmin()) {
      router.push('/menu');
      return;
    }
  }, [isAuthenticated, isAdmin, router]);

  useEffect(() => {
    const updateDateTime = () => {
      const now = new Date();
      const date = now.toLocaleDateString('en-US', { month: '2-digit', day: '2-digit', year: '2-digit' });
      const time = now.toLocaleTimeString('en-US', { hour: '2-digit', minute: '2-digit', second: '2-digit', hour12: false });
      setCurrentDateTime(`${date} ${time}`);
    };

    updateDateTime();
    const interval = setInterval(updateDateTime, 1000);
    return () => clearInterval(interval);
  }, []);

  const menuOptions = [
    { number: 1, name: 'User List (Security)', path: '/admin/users', description: 'View and manage all users' },
    { number: 2, name: 'User Add (Security)', path: '/admin/users/add', description: 'Add new user to the system' },
    { number: 3, name: 'User Update (Security)', path: '/admin/users/update', description: 'Update existing user information' },
    { number: 4, name: 'User Delete (Security)', path: '/admin/users/delete', description: 'Delete user from the system' },
  ];

  const handleOptionSelect = (path: string) => {
    router.push(path);
  };

  const handleLogout = () => {
    logout();
    router.push('/login');
  };

  if (!user) {
    return null;
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-gray-50 to-gray-100">
      {/* Header */}
      <div className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white shadow-lg">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold">CA00 - COADM01C</h1>
              <p className="text-blue-100 text-sm">CardDemo Application - Administrative Menu</p>
            </div>
            <div className="text-right">
              <p className="text-sm text-blue-100">{currentDateTime}</p>
              <p className="text-sm font-medium">{user.firstName} {user.lastName}</p>
            </div>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        <div className="bg-white rounded-lg shadow-xl overflow-hidden">
          <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-800">Administrative Functions</h2>
            <p className="text-sm text-gray-600 mt-1">Select an option to continue</p>
          </div>

          <div className="p-6">
            <div className="space-y-3">
              {menuOptions.map((option) => (
                <button
                  key={option.number}
                  onClick={() => handleOptionSelect(option.path)}
                  className="w-full text-left p-4 rounded-lg border-2 border-gray-200 hover:border-blue-500 hover:bg-blue-50 transition-all duration-200 group"
                >
                  <div className="flex items-start">
                    <span className="flex-shrink-0 w-8 h-8 bg-blue-600 text-white rounded-full flex items-center justify-center font-semibold group-hover:bg-blue-700">
                      {option.number}
                    </span>
                    <div className="ml-4 flex-1">
                      <h3 className="text-lg font-medium text-gray-900 group-hover:text-blue-700">
                        {option.name}
                      </h3>
                      <p className="text-sm text-gray-600 mt-1">{option.description}</p>
                    </div>
                    <svg className="w-5 h-5 text-gray-400 group-hover:text-blue-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 5l7 7-7 7" />
                    </svg>
                  </div>
                </button>
              ))}
            </div>
          </div>

          <div className="bg-gray-50 px-6 py-4 border-t border-gray-200">
            <div className="flex justify-between items-center">
              <p className="text-sm text-gray-600">
                Press <kbd className="px-2 py-1 bg-white border border-gray-300 rounded text-xs">PF3</kbd> to sign out
              </p>
              <Button variant="secondary" onClick={handleLogout}>
                Sign Out
              </Button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
