'use client';

import React, { useState, useEffect, useCallback } from 'react';
import Link from 'next/link';
import Image from 'next/image';
import { Button } from '@/components/ui/Button';

interface HomePageState {
  currentTime: string;
  showWelcome: boolean;
  systemStatus: 'online' | 'maintenance' | 'offline';
  userCount: number;
  lastUpdated: string;
}

export default function HomePage() {
  const [state, setState] = useState<HomePageState>({
    currentTime: '',
    showWelcome: true,
    systemStatus: 'online',
    userCount: 0,
    lastUpdated: '',
  });

  const updateState = useCallback((updates: Partial<HomePageState>) => {
    setState(prev => ({ ...prev, ...updates }));
  }, []);

  const updateCurrentTime = useCallback(() => {
    const now = new Date();
    const timeString = now.toLocaleString('en-US', {
      weekday: 'long',
      year: 'numeric',
      month: 'long',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit',
      second: '2-digit',
      hour12: true,
    });
    updateState({ currentTime: timeString });
  }, [updateState]);

  const handleNavigateToLogin = useCallback(() => {
    window.location.href = '/login';
  }, []);

  const handleNavigateToUsers = useCallback(() => {
    window.location.href = '/users';
  }, []);

  const handleNavigateToAdmin = useCallback(() => {
    window.location.href = '/admin';
  }, []);

  const handleSystemInfo = useCallback(() => {
    alert('User Management System v2.1.0\nBuilt with Next.js 15.5.3 and React 19\nCopyright © 2025 System Modernization Team');
  }, []);

  const handleExit = useCallback(() => {
    const confirmExit = window.confirm('Are you sure you want to exit the application?');
    if (confirmExit) {
      window.close();
    }
  }, []);

  useEffect(() => {
    // Update time immediately and then every second
    updateCurrentTime();
    const timeInterval = setInterval(updateCurrentTime, 1000);

    // Simulate system status check
    const statusInterval = setInterval(() => {
      updateState({ 
        lastUpdated: new Date().toLocaleTimeString(),
        userCount: Math.floor(Math.random() * 150) + 50 // Simulate user count
      });
    }, 30000); // Update every 30 seconds

    // Initial system data
    updateState({
      lastUpdated: new Date().toLocaleTimeString(),
      userCount: Math.floor(Math.random() * 150) + 50,
    });

    return () => {
      clearInterval(timeInterval);
      clearInterval(statusInterval);
    };
  }, [updateCurrentTime, updateState]);

  useEffect(() => {
    const handleKeyDown = (event: KeyboardEvent) => {
      switch (event.key) {
        case 'F1':
          event.preventDefault();
          handleSystemInfo();
          break;
        case 'F2':
          event.preventDefault();
          handleNavigateToLogin();
          break;
        case 'F3':
          event.preventDefault();
          handleExit();
          break;
        case 'F5':
          event.preventDefault();
          handleNavigateToUsers();
          break;
        case 'F6':
          event.preventDefault();
          handleNavigateToAdmin();
          break;
        case 'Enter':
          event.preventDefault();
          handleNavigateToLogin();
          break;
        default:
          break;
      }
    };

    document.addEventListener('keydown', handleKeyDown);
    return () => {
      document.removeEventListener('keydown', handleKeyDown);
    };
  }, [handleSystemInfo, handleNavigateToLogin, handleExit, handleNavigateToUsers, handleNavigateToAdmin]);

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'online':
        return 'text-green-600 bg-green-100';
      case 'maintenance':
        return 'text-yellow-600 bg-yellow-100';
      case 'offline':
        return 'text-red-600 bg-red-100';
      default:
        return 'text-gray-600 bg-gray-100';
    }
  };

  const getStatusText = (status: string) => {
    switch (status) {
      case 'online':
        return 'System Online';
      case 'maintenance':
        return 'Under Maintenance';
      case 'offline':
        return 'System Offline';
      default:
        return 'Unknown Status';
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 to-indigo-100">
      {/* Header */}
      <header className="bg-white shadow-sm border-b border-border">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center py-4">
            <div className="flex items-center gap-4">
              <Image
                className="dark:invert"
                src="/next.svg"
                alt="Next.js logo"
                width={120}
                height={25}
                priority
              />
              <div className="w-1 h-6 bg-primary"></div>
              <h1 className="text-xl font-bold text-foreground">User Management System</h1>
            </div>
            <div className="text-sm text-muted-foreground">
              {state.currentTime}
            </div>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Welcome Section */}
        {state.showWelcome && (
          <div className="text-center mb-12">
            <h2 className="text-4xl font-bold text-foreground mb-4">
              Welcome to the User Management System
            </h2>
            <p className="text-xl text-muted-foreground max-w-3xl mx-auto">
              A comprehensive solution for managing user accounts, authentication, and system administration. 
              Access all user management functions through our secure and intuitive interface.
            </p>
          </div>
        )}

        {/* System Status */}
        <div className="mb-12">
          <div className="bg-white rounded-lg shadow-sm border border-border p-6">
            <div className="flex items-center justify-between mb-4">
              <h3 className="text-lg font-semibold text-foreground">System Status</h3>
              <div className={`px-3 py-1 rounded-full text-sm font-medium ${getStatusColor(state.systemStatus)}`}>
                {getStatusText(state.systemStatus)}
              </div>
            </div>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
              <div className="text-center p-4 bg-muted/30 rounded-md">
                <div className="text-2xl font-bold text-primary">{state.userCount}</div>
                <div className="text-sm text-muted-foreground">Active Users</div>
              </div>
              <div className="text-center p-4 bg-muted/30 rounded-md">
                <div className="text-2xl font-bold text-primary">99.9%</div>
                <div className="text-sm text-muted-foreground">System Uptime</div>
              </div>
              <div className="text-center p-4 bg-muted/30 rounded-md">
                <div className="text-2xl font-bold text-primary">v2.1.0</div>
                <div className="text-sm text-muted-foreground">System Version</div>
              </div>
            </div>
            <div className="mt-4 text-xs text-muted-foreground text-center">
              Last updated: {state.lastUpdated}
            </div>
          </div>
        </div>

        {/* Navigation Cards */}
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8 mb-12">
          {/* Login Card */}
          <div className="bg-white rounded-lg shadow-sm border border-border p-6 hover:shadow-md transition-shadow">
            <div className="flex items-center gap-3 mb-4">
              <div className="w-12 h-12 bg-primary/10 rounded-lg flex items-center justify-center">
                <span className="text-primary text-xl">🔐</span>
              </div>
              <div>
                <h3 className="text-lg font-semibold text-foreground">System Login</h3>
                <p className="text-sm text-muted-foreground">COUSR01C</p>
              </div>
            </div>
            <p className="text-muted-foreground mb-4">
              Secure authentication portal for system access. Enter your credentials to access user management functions.
            </p>
            <Button
              onClick={handleNavigateToLogin}
              variant="primary"
              className="w-full"
            >
              Login (F2)
            </Button>
          </div>

          {/* User Management Card */}
          <div className="bg-white rounded-lg shadow-sm border border-border p-6 hover:shadow-md transition-shadow">
            <div className="flex items-center gap-3 mb-4">
              <div className="w-12 h-12 bg-blue-100 rounded-lg flex items-center justify-center">
                <span className="text-blue-600 text-xl">👥</span>
              </div>
              <div>
                <h3 className="text-lg font-semibold text-foreground">User Management</h3>
                <p className="text-sm text-muted-foreground">COMEN01C</p>
              </div>
            </div>
            <p className="text-muted-foreground mb-4">
              Comprehensive user account management including creation, modification, and deletion of user records.
            </p>
            <Button
              onClick={handleNavigateToUsers}
              variant="outline"
              className="w-full"
            >
              User Management (F5)
            </Button>
          </div>

          {/* Admin Panel Card */}
          <div className="bg-white rounded-lg shadow-sm border border-border p-6 hover:shadow-md transition-shadow">
            <div className="flex items-center gap-3 mb-4">
              <div className="w-12 h-12 bg-purple-100 rounded-lg flex items-center justify-center">
                <span className="text-purple-600 text-xl">⚙️</span>
              </div>
              <div>
                <h3 className="text-lg font-semibold text-foreground">Administration</h3>
                <p className="text-sm text-muted-foreground">COADM01C</p>
              </div>
            </div>
            <p className="text-muted-foreground mb-4">
              Advanced system administration tools and settings. Requires administrator privileges for access.
            </p>
            <Button
              onClick={handleNavigateToAdmin}
              variant="outline"
              className="w-full"
            >
              Admin Panel (F6)
            </Button>
          </div>
        </div>

        {/* System Features */}
        <div className="bg-white rounded-lg shadow-sm border border-border p-8 mb-12">
          <h3 className="text-2xl font-bold text-foreground mb-6 text-center">System Features</h3>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
            <div className="text-center">
              <div className="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-3">
                <span className="text-green-600 text-2xl">✓</span>
              </div>
              <h4 className="font-semibold text-foreground mb-2">Secure Authentication</h4>
              <p className="text-sm text-muted-foreground">
                Multi-level security with role-based access control
              </p>
            </div>
            <div className="text-center">
              <div className="w-16 h-16 bg-blue-100 rounded-full flex items-center justify-center mx-auto mb-3">
                <span className="text-blue-600 text-2xl">📊</span>
              </div>
              <h4 className="font-semibold text-foreground mb-2">User Analytics</h4>
              <p className="text-sm text-muted-foreground">
                Comprehensive reporting and user activity tracking
              </p>
            </div>
            <div className="text-center">
              <div className="w-16 h-16 bg-purple-100 rounded-full flex items-center justify-center mx-auto mb-3">
                <span className="text-purple-600 text-2xl">🔧</span>
              </div>
              <h4 className="font-semibold text-foreground mb-2">Easy Management</h4>
              <p className="text-sm text-muted-foreground">
                Intuitive interface for all user management operations
              </p>
            </div>
            <div className="text-center">
              <div className="w-16 h-16 bg-orange-100 rounded-full flex items-center justify-center mx-auto mb-3">
                <span className="text-orange-600 text-2xl">🚀</span>
              </div>
              <h4 className="font-semibold text-foreground mb-2">High Performance</h4>
              <p className="text-sm text-muted-foreground">
                Fast, reliable, and scalable user management solution
              </p>
            </div>
          </div>
        </div>

        {/* Quick Actions */}
        <div className="bg-white rounded-lg shadow-sm border border-border p-6">
          <h3 className="text-lg font-semibold text-foreground mb-4">Quick Actions</h3>
          <div className="flex flex-wrap gap-3">
            <Button
              onClick={handleNavigateToLogin}
              variant="primary"
              size="sm"
            >
              Login (ENTER)
            </Button>
            <Button
              onClick={handleSystemInfo}
              variant="outline"
              size="sm"
            >
              System Info (F1)
            </Button>
            <Button
              onClick={handleNavigateToUsers}
              variant="outline"
              size="sm"
            >
              Users (F5)
            </Button>
            <Button
              onClick={handleNavigateToAdmin}
              variant="outline"
              size="sm"
            >
              Admin (F6)
            </Button>
            <Button
              onClick={handleExit}
              variant="outline"
              size="sm"
            >
              Exit (F3)
            </Button>
          </div>
        </div>
      </main>

      {/* Footer */}
      <footer className="bg-white border-t border-border mt-12">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
            <div>
              <h4 className="font-semibold text-foreground mb-3">System Information</h4>
              <div className="space-y-2 text-sm text-muted-foreground">
                <p>Version: 2.1.0</p>
                <p>Build: Next.js 15.5.3</p>
                <p>Framework: React 19</p>
                <p>Last Updated: {new Date().toLocaleDateString()}</p>
              </div>
            </div>
            <div>
              <h4 className="font-semibold text-foreground mb-3">Support</h4>
              <div className="space-y-2 text-sm text-muted-foreground">
                <p>Help Desk: Available 24/7</p>
                <p>Documentation: Online Help</p>
                <p>Training: User Guides Available</p>
                <p>Technical Support: IT Department</p>
              </div>
            </div>
            <div>
              <h4 className="font-semibold text-foreground mb-3">Security Notice</h4>
              <div className="space-y-2 text-sm text-muted-foreground">
                <p>All activities are logged and monitored</p>
                <p>Unauthorized access is prohibited</p>
                <p>Report security issues immediately</p>
                <p>Follow company security policies</p>
              </div>
            </div>
          </div>
          <div className="border-t border-border mt-8 pt-6 text-center">
            <p className="text-sm text-muted-foreground">
              © 2025 User Management System. All rights reserved. | 
              <span className="ml-2">Developed by System Modernization Team</span>
            </p>
          </div>
        </div>
      </footer>

      {/* Keyboard Shortcuts Help */}
      <div className="fixed bottom-4 right-4 bg-white border border-border rounded-lg p-3 shadow-lg">
        <p className="text-xs text-muted-foreground font-medium mb-1">Keyboard Shortcuts:</p>
        <div className="text-xs text-muted-foreground space-y-1">
          <p>F1 = System Info | F2 = Login | F3 = Exit</p>
          <p>F5 = Users | F6 = Admin | ENTER = Login</p>
        </div>
      </div>
    </div>
  );
}