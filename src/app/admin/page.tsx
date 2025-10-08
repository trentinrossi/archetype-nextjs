'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

interface AdminDashboardCard {
  id: string;
  title: string;
  description: string;
  icon: string;
  route: string;
  color: string;
  hoverColor: string;
}

const ADMIN_FUNCTIONS: AdminDashboardCard[] = [
  {
    id: 'view-users',
    title: 'User List',
    description: 'Browse, search, and manage all users in the system',
    icon: '👥',
    route: '/users',
    color: 'bg-blue-50 border-blue-200',
    hoverColor: 'hover:bg-blue-100'
  },
  {
    id: 'add-user',
    title: 'Add User',
    description: 'Create a new user account',
    icon: '➕',
    route: '/users/add',
    color: 'bg-green-50 border-green-200',
    hoverColor: 'hover:bg-green-100'
  }
];

export default function AdminDashboardPage() {
  const router = useRouter();

  const handleNavigate = (route: string) => {
    router.push(route);
  };

  const handleKeyDown = (event: KeyboardEvent) => {
    switch (event.key) {
      case 'F1':
        event.preventDefault();
        handleNavigate('/users');
        break;
      
      case 'F2':
        event.preventDefault();
        handleNavigate('/users/add');
        break;
      
      case 'F12':
        event.preventDefault();
        router.push('/');
        break;
      
      default:
        if (event.key.startsWith('F') && !['F1', 'F2', 'F12'].includes(event.key)) {
          event.preventDefault();
        }
        break;
    }
  };

  React.useEffect(() => {
    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, []);

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-6xl mx-auto">
        {/* Header Section */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-8">
          <div className="flex justify-between items-center">
            <div>
              <h1 className="text-2xl font-bold text-gray-900 mb-2">CARDDEMO</h1>
              <h2 className="text-lg text-gray-700">Administration Menu</h2>
            </div>
            <div className="text-right text-sm text-gray-600">
              <div>Program: COADM01C</div>
              <div>Transaction: CADM</div>
              <div>{new Date().toLocaleDateString()}</div>
              <div>{new Date().toLocaleTimeString()}</div>
            </div>
          </div>
          
          <div className="mt-4 bg-blue-50 border border-blue-200 rounded-lg p-4">
            <h3 className="text-sm font-medium text-blue-900 mb-2">Welcome to the Administration Panel</h3>
            <p className="text-sm text-blue-800">
              Select a function below to manage users in the system. Use the navigation cards or function keys for quick access.
            </p>
          </div>
        </div>

        {/* Function Cards Grid */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
          {ADMIN_FUNCTIONS.map((func, index) => (
            <div
              key={func.id}
              className={`${func.color} ${func.hoverColor} border rounded-lg p-6 cursor-pointer transition-all duration-200 hover:shadow-md`}
              onClick={() => handleNavigate(func.route)}
            >
              <div className="flex items-start gap-4">
                <div className="text-4xl">{func.icon}</div>
                <div className="flex-1">
                  <h3 className="text-xl font-semibold text-gray-900 mb-2">
                    {func.title}
                  </h3>
                  <p className="text-gray-700 mb-4">
                    {func.description}
                  </p>
                  <div className="flex items-center justify-between">
                    <Button
                      onClick={(e) => {
                        e.stopPropagation();
                        handleNavigate(func.route);
                      }}
                      className="bg-white hover:bg-gray-50 text-gray-700 border border-gray-300"
                    >
                      Open Function
                    </Button>
                    <kbd className="px-3 py-1 bg-gray-100 text-gray-600 rounded font-mono">
                      F{index + 1}
                    </kbd>
                  </div>
                </div>
              </div>
            </div>
          ))}
        </div>

        {/* Quick Navigation */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-8">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Quick Navigation</h3>
          <div className="flex flex-wrap gap-3">
            <Button
              onClick={() => handleNavigate('/users')}
              className="bg-blue-600 hover:bg-blue-700 text-white flex items-center gap-2"
            >
              <span>👥</span>
              User List (F1)
            </Button>
            
            <Button
              onClick={() => handleNavigate('/users/add')}
              className="bg-green-600 hover:bg-green-700 text-white flex items-center gap-2"
            >
              <span>➕</span>
              Add User (F2)
            </Button>
            
            <Button
              onClick={() => router.push('/')}
              variant="outline"
              className="text-gray-600 hover:text-gray-800 flex items-center gap-2 ml-auto"
            >
              <span>🏠</span>
              Home (F12)
            </Button>
          </div>
        </div>

        {/* System Information */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-8">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">System Information</h3>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div className="bg-blue-50 border border-blue-200 rounded-lg p-4 text-center">
              <div className="text-2xl font-bold text-blue-600">👥</div>
              <div className="text-sm text-blue-800 font-medium mt-1">User Management</div>
              <div className="text-xs text-blue-600 mt-1">Complete CRUD Operations</div>
            </div>
            
            <div className="bg-green-50 border border-green-200 rounded-lg p-4 text-center">
              <div className="text-2xl font-bold text-green-600">✅</div>
              <div className="text-sm text-green-800 font-medium mt-1">System Status</div>
              <div className="text-xs text-green-600 mt-1">All Functions Available</div>
            </div>
            
            <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4 text-center">
              <div className="text-2xl font-bold text-yellow-600">🔐</div>
              <div className="text-sm text-yellow-800 font-medium mt-1">Security</div>
              <div className="text-xs text-yellow-600 mt-1">Admin Access Required</div>
            </div>
          </div>
        </div>

        {/* Help Section */}
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-6">
          <h4 className="text-lg font-semibold text-blue-900 mb-4">Function Key Commands</h4>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div className="space-y-2">
              <div className="flex items-center gap-3">
                <kbd className="px-3 py-1 bg-blue-100 text-blue-800 rounded font-mono">F1</kbd>
                <span className="text-blue-800">User List - View and manage all users</span>
              </div>
              <div className="flex items-center gap-3">
                <kbd className="px-3 py-1 bg-blue-100 text-blue-800 rounded font-mono">F2</kbd>
                <span className="text-blue-800">Add User - Create new user account</span>
              </div>
            </div>
            <div className="space-y-2">
              <div className="flex items-center gap-3">
                <kbd className="px-3 py-1 bg-blue-100 text-blue-800 rounded font-mono">F12</kbd>
                <span className="text-blue-800">Home - Return to main application</span>
              </div>
            </div>
          </div>
          
          <div className="mt-4 pt-4 border-t border-blue-200">
            <p className="text-blue-800 text-sm">
              <strong>Navigation:</strong> Click on any function card or use the corresponding function key for quick access to user management features.
            </p>
          </div>
        </div>
      </div>
    </div>
  );
}