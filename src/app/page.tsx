'use client';

import React from 'react';
import { useRouter } from 'next/navigation';
import { Button } from '@/components/ui';

export default function HomePage() {
  const router = useRouter();

  const handleNavigateToAdmin = () => {
    router.push('/admin');
  };

  const handleNavigateToUsers = () => {
    router.push('/users');
  };

  const handleKeyDown = (event: KeyboardEvent) => {
    switch (event.key) {
      case 'F1':
        event.preventDefault();
        handleNavigateToAdmin();
        break;
      
      case 'F2':
        event.preventDefault();
        handleNavigateToUsers();
        break;
      
      default:
        if (event.key.startsWith('F') && !['F1', 'F2'].includes(event.key)) {
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
      <div className="max-w-4xl mx-auto">
        {/* Header Section */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-8 mb-8 text-center">
          <div className="mb-6">
            <h1 className="text-4xl font-bold text-gray-900 mb-4">CARDDEMO</h1>
            <h2 className="text-xl text-gray-700 mb-2">User Management System</h2>
            <p className="text-gray-600">
              A comprehensive system for managing user accounts and administration
            </p>
          </div>
          
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <p className="text-blue-800 text-sm">
              Welcome to the CardDemo User Management System. Select an option below to get started.
            </p>
          </div>
        </div>

        {/* Main Navigation Cards */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-8">
          {/* Admin Dashboard Card */}
          <div
            className="bg-blue-50 border border-blue-200 hover:bg-blue-100 rounded-lg p-6 cursor-pointer transition-all duration-200 hover:shadow-md"
            onClick={handleNavigateToAdmin}
          >
            <div className="text-center">
              <div className="text-5xl mb-4">⚙️</div>
              <h3 className="text-xl font-semibold text-gray-900 mb-3">
                Administration Menu
              </h3>
              <p className="text-gray-700 mb-4">
                Access the full administration panel with all user management functions
              </p>
              <div className="flex items-center justify-between">
                <Button
                  onClick={(e) => {
                    e.stopPropagation();
                    handleNavigateToAdmin();
                  }}
                  className="bg-blue-600 hover:bg-blue-700 text-white"
                >
                  Enter Admin Panel
                </Button>
                <kbd className="px-3 py-1 bg-blue-100 text-blue-600 rounded font-mono">
                  F1
                </kbd>
              </div>
            </div>
          </div>

          {/* User List Card */}
          <div
            className="bg-green-50 border border-green-200 hover:bg-green-100 rounded-lg p-6 cursor-pointer transition-all duration-200 hover:shadow-md"
            onClick={handleNavigateToUsers}
          >
            <div className="text-center">
              <div className="text-5xl mb-4">👥</div>
              <h3 className="text-xl font-semibold text-gray-900 mb-3">
                User List
              </h3>
              <p className="text-gray-700 mb-4">
                Directly access the user list to view, search, and manage users
              </p>
              <div className="flex items-center justify-between">
                <Button
                  onClick={(e) => {
                    e.stopPropagation();
                    handleNavigateToUsers();
                  }}
                  className="bg-green-600 hover:bg-green-700 text-white"
                >
                  View Users
                </Button>
                <kbd className="px-3 py-1 bg-green-100 text-green-600 rounded font-mono">
                  F2
                </kbd>
              </div>
            </div>
          </div>
        </div>

        {/* Quick Actions */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-8">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">Quick Actions</h3>
          <div className="flex flex-wrap gap-3">
            <Button
              onClick={handleNavigateToAdmin}
              className="bg-blue-600 hover:bg-blue-700 text-white flex items-center gap-2"
            >
              <span>⚙️</span>
              Admin Menu (F1)
            </Button>
            
            <Button
              onClick={handleNavigateToUsers}
              className="bg-green-600 hover:bg-green-700 text-white flex items-center gap-2"
            >
              <span>👥</span>
              User List (F2)
            </Button>
            
            <Button
              onClick={() => router.push('/users/add')}
              className="bg-yellow-600 hover:bg-yellow-700 text-white flex items-center gap-2"
            >
              <span>➕</span>
              Add User
            </Button>
          </div>
        </div>

        {/* System Features */}
        <div className="bg-white rounded-lg shadow-sm border border-gray-200 p-6 mb-8">
          <h3 className="text-lg font-semibold text-gray-900 mb-4">System Features</h3>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            <div className="text-center p-4 bg-blue-50 border border-blue-200 rounded-lg">
              <div className="text-2xl mb-2">📋</div>
              <div className="text-sm font-medium text-blue-800">User List</div>
              <div className="text-xs text-blue-600 mt-1">Browse & Search</div>
            </div>
            
            <div className="text-center p-4 bg-green-50 border border-green-200 rounded-lg">
              <div className="text-2xl mb-2">➕</div>
              <div className="text-sm font-medium text-green-800">Add Users</div>
              <div className="text-xs text-green-600 mt-1">Create Accounts</div>
            </div>
            
            <div className="text-center p-4 bg-yellow-50 border border-yellow-200 rounded-lg">
              <div className="text-2xl mb-2">✏️</div>
              <div className="text-sm font-medium text-yellow-800">Update Users</div>
              <div className="text-xs text-yellow-600 mt-1">Modify Information</div>
            </div>
            
            <div className="text-center p-4 bg-red-50 border border-red-200 rounded-lg">
              <div className="text-2xl mb-2">🗑️</div>
              <div className="text-sm font-medium text-red-800">Delete Users</div>
              <div className="text-xs text-red-600 mt-1">Remove Accounts</div>
            </div>
          </div>
        </div>

        {/* Help Section */}
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-6">
          <h4 className="text-lg font-semibold text-blue-900 mb-4">Getting Started</h4>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <h5 className="font-medium text-blue-800 mb-2">Function Keys</h5>
              <div className="space-y-1 text-sm text-blue-700">
                <div className="flex items-center gap-2">
                  <kbd className="px-2 py-1 bg-blue-100 rounded text-xs font-mono">F1</kbd>
                  <span>Administration Menu</span>
                </div>
                <div className="flex items-center gap-2">
                  <kbd className="px-2 py-1 bg-blue-100 rounded text-xs font-mono">F2</kbd>
                  <span>User List</span>
                </div>
              </div>
            </div>
            <div>
              <h5 className="font-medium text-blue-800 mb-2">Navigation</h5>
              <div className="text-sm text-blue-700 space-y-1">
                <div>• Click on cards to navigate</div>
                <div>• Use function keys for quick access</div>
                <div>• Follow on-screen instructions</div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}