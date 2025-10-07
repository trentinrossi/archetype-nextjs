'use client';

import React, { useState, useEffect } from 'react';
import { useRouter } from 'next/navigation';
import { userSecurityService } from '@/services/userSecurityService';

const AdminDashboardPage: React.FC = () => {
  const router = useRouter();
  const [isLoading, setIsLoading] = useState<boolean>(true);
  const [currentUser, setCurrentUser] = useState<any>(null);

  useEffect(() => {
    // Check authentication and user type
    const session = userSecurityService.getCurrentSession();
    
    if (!session || !userSecurityService.isAuthenticated()) {
      router.push('/');
      return;
    }

    if (session.userType !== 'ADMIN') {
      router.push('/user/dashboard');
      return;
    }

    setCurrentUser(session);
    setIsLoading(false);
  }, [router]);

  const handleSignOut = async () => {
    try {
      await userSecurityService.exit();
      router.push('/');
    } catch (error) {
      console.error('Sign out error:', error);
      router.push('/');
    }
  };

  const navigateToUserList = () => {
    router.push('/admin/users');
  };

  const navigateToUserAdd = () => {
    router.push('/admin/users/add');
  };

  const handleKeyDown = (event: React.KeyboardEvent) => {
    switch (event.key) {
      case 'F3':
        event.preventDefault();
        handleSignOut();
        break;
      default:
        break;
    }
  };

  if (isLoading) {
    return (
      <div className="min-h-screen bg-gray-50 flex items-center justify-center">
        <div className="text-center">
          <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-indigo-600 mx-auto"></div>
          <p className="mt-4 text-gray-600">Loading dashboard...</p>
        </div>
      </div>
    );
  }

  return (
    <div 
      className="min-h-screen bg-gray-50"
      onKeyDown={handleKeyDown}
      tabIndex={-1}
    >
      {/* Header */}
      <div className="bg-white shadow">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center py-6">
            <div>
              <h1 className="text-2xl font-bold text-gray-900">CardDemo</h1>
              <p className="text-sm text-gray-600">Administration Menu - COADM01C</p>
            </div>
            <div className="flex items-center space-x-4">
              <div className="text-right">
                <p className="text-sm font-medium text-gray-900">
                  Welcome, {currentUser?.userName || currentUser?.userId}
                </p>
                <p className="text-xs text-gray-500">Administrator</p>
              </div>
              <button
                onClick={handleSignOut}
                className="bg-gray-600 text-white px-4 py-2 rounded-md text-sm hover:bg-gray-700 focus:outline-none focus:ring-2 focus:ring-gray-500"
              >
                Sign Out (F3)
              </button>
            </div>
          </div>
        </div>
      </div>

      {/* Main Content */}
      <div className="max-w-7xl mx-auto py-6 sm:px-6 lg:px-8">
        <div className="px-4 py-6 sm:px-0">
          <div className="mb-8">
            <h2 className="text-xl font-semibold text-gray-900 mb-2">
              User Management System
            </h2>
            <p className="text-gray-600">
              Manage user accounts, permissions, and security settings.
            </p>
          </div>

          {/* Menu Options */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {/* User List */}
            <div 
              onClick={navigateToUserList}
              className="bg-white overflow-hidden shadow rounded-lg cursor-pointer hover:shadow-md transition-shadow duration-200"
            >
              <div className="p-6">
                <div className="flex items-center">
                  <div className="flex-shrink-0">
                    <svg className="h-8 w-8 text-indigo-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" />
                    </svg>
                  </div>
                  <div className="ml-5 w-0 flex-1">
                    <dl>
                      <dt className="text-sm font-medium text-gray-500 truncate">
                        User List
                      </dt>
                      <dd className="text-lg font-medium text-gray-900">
                        COUSR00C
                      </dd>
                    </dl>
                  </div>
                </div>
                <div className="mt-4">
                  <p className="text-sm text-gray-600">
                    View, search, and manage existing user accounts. Update or delete user records.
                  </p>
                </div>
              </div>
            </div>

            {/* Add User */}
            <div 
              onClick={navigateToUserAdd}
              className="bg-white overflow-hidden shadow rounded-lg cursor-pointer hover:shadow-md transition-shadow duration-200"
            >
              <div className="p-6">
                <div className="flex items-center">
                  <div className="flex-shrink-0">
                    <svg className="h-8 w-8 text-green-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M18 9v3m0 0v3m0-3h3m-3 0h-3m-2-5a4 4 0 11-8 0 4 4 0 018 0zM3 20a6 6 0 0112 0v1H3v-1z" />
                    </svg>
                  </div>
                  <div className="ml-5 w-0 flex-1">
                    <dl>
                      <dt className="text-sm font-medium text-gray-500 truncate">
                        Add User
                      </dt>
                      <dd className="text-lg font-medium text-gray-900">
                        COUSR01C
                      </dd>
                    </dl>
                  </div>
                </div>
                <div className="mt-4">
                  <p className="text-sm text-gray-600">
                    Create new user accounts with appropriate permissions and access levels.
                  </p>
                </div>
              </div>
            </div>

            {/* System Information */}
            <div className="bg-white overflow-hidden shadow rounded-lg">
              <div className="p-6">
                <div className="flex items-center">
                  <div className="flex-shrink-0">
                    <svg className="h-8 w-8 text-blue-600" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                    </svg>
                  </div>
                  <div className="ml-5 w-0 flex-1">
                    <dl>
                      <dt className="text-sm font-medium text-gray-500 truncate">
                        System Info
                      </dt>
                      <dd className="text-lg font-medium text-gray-900">
                        Status
                      </dd>
                    </dl>
                  </div>
                </div>
                <div className="mt-4">
                  <p className="text-sm text-gray-600">
                    View system status, version information, and administrative details.
                  </p>
                </div>
              </div>
            </div>
          </div>

          {/* System Status */}
          <div className="mt-8 bg-white shadow rounded-lg">
            <div className="px-6 py-4 border-b border-gray-200">
              <h3 className="text-lg font-medium text-gray-900">System Status</h3>
            </div>
            <div className="px-6 py-4">
              <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                <div className="text-center">
                  <div className="text-2xl font-bold text-green-600">Online</div>
                  <div className="text-sm text-gray-500">System Status</div>
                </div>
                <div className="text-center">
                  <div className="text-2xl font-bold text-blue-600">{new Date().toLocaleDateString()}</div>
                  <div className="text-sm text-gray-500">Current Date</div>
                </div>
                <div className="text-center">
                  <div className="text-2xl font-bold text-purple-600">{new Date().toLocaleTimeString()}</div>
                  <div className="text-sm text-gray-500">Current Time</div>
                </div>
              </div>
            </div>
          </div>

          {/* Function Key Instructions */}
          <div className="mt-8 bg-gray-50 border border-gray-200 rounded-md p-4">
            <h3 className="text-sm font-medium text-gray-900 mb-2">Function Keys</h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-2 text-xs text-gray-600">
              <div><span className="font-medium">F3:</span> Sign out and exit</div>
              <div><span className="font-medium">Click:</span> Navigate to menu options</div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default AdminDashboardPage;